package core

import chisel3._
import chisel3.util._

case class ICacheConfig(
    totalSize: Int = 4 * 16, // Bytes
    ways: Int = 1
)

sealed trait HasICacheConst {
  implicit val cacheConfig: ICacheConfig

  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = 4 * 4 // TODO: byte
  val LineBeats = LineSize / 4 // DATA WIDTH 32
  val Sets = TotalSize / LineSize / Ways
  val OffsetBits = log2Up(LineSize) // 26 6 2
  val IndexBits = log2Up(Sets)
  val WordIndexBits = log2Up(LineBeats) // TODO
  val TagBits = 32 - OffsetBits - IndexBits

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val WordIndex = UInt(WordIndexBits.W)
    val byteOffset = UInt(2.W)
  }

  def getMataIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) =
    Cat(getMataIdx(addr), addr.asTypeOf(addrBundle).WordIndex)

  def isSameWorld(a1: UInt, a2: UInt) = ((a1 >> 2) === (a2 >> 2))
  def isSetConflict(a1: UInt, a2: UInt) =
    (a1.asTypeOf(addrBundle).index === a2.asTypeOf(addrBundle).index)
}

sealed abstract class ICacheBundle(implicit cacheConfig: ICacheConfig)
    extends Bundle
    with HasICacheConst

sealed abstract class ICacheModule(implicit cacheConfig: ICacheConfig)
    extends Module
    with HasICacheConst

sealed class ICacheMetaBundle(implicit val cacheConfig: ICacheConfig)
    extends ICacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(UInt(1.W))

  def apply(tag: UInt, valid: UInt) = {
    this.tag := tag
    this.valid := valid
    this
  }
}

sealed class ICacheDataBundle(implicit val cacheConfig: ICacheConfig)
    extends ICacheBundle {
  val data = Output(UInt(32.W)) // DataBits

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

object ICachePipelineConnect {
  def apply[T <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool
  ) = {
    val s_idle :: s_in  :: Nil = Enum(2)
    val state = RegInit(s_idle)

    val reg = RegEnable(left.bits, left.valid && right.ready)
    state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(left.valid && right.ready, s_in, s_idle),
      s_in -> Mux(rightOutFire, Mux(left.valid && right.ready, s_in, s_idle), s_in)
    ))

    // orginal code
    /*
    when(state === s_in) {
      right.bits := reg
    }.otherwise{
      right.bits := reg
      right.bits match {
        case b: IDU2EXU => b.regW := false.B
        case b: EXU2LSU => b.regW := false.B
        case _ => // 如果不是 IDU2EXU 或 EXU2LSU，不做任何操作
      }
      right.bits match {
        case b: EXU2LSU => b.needMem := false.B
        case _ => 
      }
    }
    */
    // new code
    right.bits := reg

    val valid = RegInit(false.B)
    when(rightOutFire) { valid := false.B } // already excepted, right.valid := false
    when(left.valid && right.ready) { valid := true.B } // in.fire
    when(isFlush) { valid := false.B }

    left.ready := right.ready
    // right.bits := RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush
  }
}

class ICache(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle {
    val axi = new AXI
    val ready = Input(Bool())
    val inst = Output(UInt(32.W))
    val instValid = Output(Bool())
    val outFire = Input(Bool())
    val addr = Input(UInt(32.W))
    val addrValid = Input(Bool())
    val fenceI = Input(Bool())
    val refetch = Input(Bool())
  })
  io.axi := DontCare // walk around
  val addr = io.addr.asTypeOf(addrBundle)
  
  val metaArray = SyncReadMem(Sets, Vec(Ways, new ICacheMetaBundle))
  val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))

  // see 《SuperScalar RISC Processor Design》 P24
  // 000     001(Tag Access)  010          011          100(Data Access) 101(Result Drive) 110 
  val s_idle :: s_judge :: s_fetching :: s_wait_data :: s_data_access :: s_valid :: s_error :: Nil = Enum(7)
  val state = RegInit(s_idle)

  val hitVec = VecInit(
    metaArray(addr.index).map(m => m.tag === addr.tag && m.valid === 1.U)
  ).asUInt
  val hit = hitVec.orR 
  val hitfake = hitVec.orR
  dontTouch(hitfake)
  dontTouch(hit)

  val cacheData = Wire(UInt(32.W))
  cacheData := DontCare
  for (i <- 0 until Ways)
    when(hitVec(i)) {
      cacheData := dataArray(addr.index)(i)(addr.WordIndex) // TODO
    }
  io.inst := cacheData

  val refetchLatch = RegInit(false.B)
  when(io.refetch) {refetchLatch := true.B} 
  when(io.axi.rlast) {refetchLatch := false.B}
  val refetch = io.refetch || refetchLatch

  // hit: idle -> judge -> data_access -> result_drive
  // miss: idle -+> judge -+> fetching -> wait_data -+---------- -------------------+-> s_judge -> s_data_access -+> result_drive
  //             |         |                         +-> s_fetching -> s_wait_data -+                             | 
  //      S1     |  S2     |                          S3                                                          |      S4 
  state := MuxLookup(state, s_idle)(
    Seq(
      s_idle -> Mux(io.addrValid, s_judge, s_idle),
      s_judge -> Mux(hit && ~refetch, s_data_access, s_fetching),
      s_fetching -> Mux(io.axi.arready, s_wait_data, s_fetching),
      s_wait_data -> Mux(io.axi.rlast, Mux(refetch, s_fetching, s_judge), s_wait_data),
      s_data_access -> s_valid,
      s_valid -> Mux(io.outFire, s_idle, s_valid) // Didn't stay
    )
  )

  // axi read signals
  io.axi.arvalid := state === s_fetching
  io.axi.araddr := Cat(io.addr(31, OffsetBits), Fill(OffsetBits, 0.U(1.W)))
  io.axi.arsize := "b010".U // burst size
  io.axi.rready := true.B
  // burst signals
  io.axi.arlen := (LineBeats - 1).asUInt // Burst_Length = AxLEN[7:0] + 1
  io.axi.arburst := "b01".U // INCR

  // save addr info for burst transation
  val burst = RegInit(0.U(WordIndexBits.W))

  io.instValid := state === s_valid
  // miss update
  val rdata = io.axi.rdata
  // rvalid in burst transaction
  when(io.axi.rvalid && state === s_wait_data) {
    burst := burst + 1.U
    dataArray(addr.index)(0)(burst) := rdata // TODO
  }
  when(io.axi.rlast && state === s_wait_data) {
    burst := 0.U
    metaArray(addr.index)(0).tag := addr.tag
    metaArray(addr.index)(0).valid := true.B
  }

  when(io.fenceI) {
    for (i <- 0 until Sets)
      for (j <- 0 until Ways)
        metaArray(i)(j).valid := false.B
  }
}

// hit: idle -> judge -> data_access -> result_drive
// miss: idle -+> judge -+> fetching -> wait_data -+---------- -------------------+-> s_judge -> s_data_access -+> result_drive
//             |         |                         +-> s_fetching -> s_wait_data -+                             | 
//      S1     |  S2     |                          S3                                                          |      S4 

// Stage1: Tag Access 
class Stage1In extends Bundle {
  val addr = Input(UInt(32.W))
  val valid = Input(Bool())
}

class Stage1Out(implicit val cacheConfig: ICacheConfig) extends ICacheBundle {
  val addr = Output(UInt(32.W))
  val wordIndex = Output(UInt(WordIndexBits.W))
  val index = Output(UInt(IndexBits.W))
  val tag = Output(UInt(TagBits.W))
}

// Stage2: Data Access
class Stage2Out(implicit val cacheConfig: ICacheConfig) extends ICacheBundle {
  val addr = Output(UInt(32.W))
  val rdata = Output(UInt(32.W))
  val hit = Output(Bool())
  val wordIndex = Output(UInt(WordIndexBits.W))
}

// Stage3: Result Drive
class Stage3Out extends Bundle {
  val addr = Output(UInt(32.W))
  val rdata = Output(UInt(32.W))
}

class metaArrayWriteBundle(implicit val cacheConfig: ICacheConfig) extends ICacheBundle {
  val index = Output(UInt(IndexBits.W))
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
}

class Stage1(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle {
    val in = new Stage1In // Flipped(Decoupled(new Stage1Out))
    val out = Decoupled(new Stage1Out)
    val metaArrayWrite = Flipped(new metaArrayWriteBundle)
    val metaArrayTag = Output(UInt(TagBits.W))
    val flush = Input(Bool())
  })

  val addr = io.in.addr.asTypeOf(addrBundle)
  val metaArray = SyncReadMem(Sets, Vec(Ways, new ICacheMetaBundle))
  val index = addr.index 
  val tag = addr.tag 
  
  io.metaArrayTag := metaArray(index)(0).tag
  when(io.metaArrayWrite.valid) {
    metaArray(io.metaArrayWrite.index)(0).tag := io.metaArrayWrite.tag
    metaArray(io.metaArrayWrite.index)(0).valid := true.B
  }

  io.out.bits.wordIndex := addr.WordIndex
  io.out.bits.addr := io.in.addr 
  io.out.bits.index := index 
  io.out.bits.tag := tag

  io.out.valid := io.in.valid && !io.flush
}

class Stage2(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1Out))
    val metaArrayTag = Input(UInt(TagBits.W))
    val out = Decoupled(new Stage2Out)
    val axi = new AXI
    val metaArrayWrite = new metaArrayWriteBundle
    val flush = Input(Bool())
    val cacheDataVec = Output(Vec(LineBeats, UInt(32.W)))
  })
  io.axi := DontCare
  val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))

  val addr = io.in.bits.addr
  val wordIndex = io.in.bits.wordIndex
  val index = io.in.bits.index 
  val tag = io.in.bits.tag

  val hit = io.metaArrayTag === io.in.bits.tag && io.in.valid
  dontTouch(hit)

  io.out.bits.hit := hit
  io.out.bits.wordIndex := wordIndex

  val cacheData = Wire(UInt(32.W))
  cacheData := dataArray(index)(0)(wordIndex)
  dontTouch(cacheData)
  val cacheDataVec = Wire(Vec(LineBeats, UInt(32.W)))
  cacheDataVec := dataArray(index)(0)
  dontTouch(cacheDataVec)
  for (i <- 0 until LineBeats) {
    io.cacheDataVec(i) := cacheDataVec(i)
  }
  io.out.bits.wordIndex := wordIndex

  // miss access
  //    00          01           10           
  val s_idle :: s_fetching :: s_wait_data :: s_valid :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val refetchLatch = RegInit(false.B)
  when(io.flush && (state =/= s_idle || !hit)) { refetchLatch := true.B }
  when(io.axi.rlast) {refetchLatch := false.B}
  val refetch = io.flush || refetchLatch

  state := MuxLookup(state, s_idle)(Seq(
    s_idle -> Mux(!hit && io.in.valid, s_fetching, s_idle),
    s_fetching -> Mux(io.axi.arready, s_wait_data, s_fetching),
    s_wait_data -> Mux(io.axi.rlast, Mux(refetch, Mux(hit, s_valid, s_fetching), s_valid), s_wait_data),
    s_valid -> s_idle
  ))
  // axi read signals
  io.axi.arvalid := state === s_fetching
  io.axi.araddr := Cat(addr(31, OffsetBits), Fill(OffsetBits, 0.U(1.W)))
  io.axi.arsize := "b010".U // burst size
  io.axi.rready := true.B
  // burst signals
  io.axi.arlen := (LineBeats - 1).asUInt // Burst_Length = AxLEN[7:0] + 1
  io.axi.arburst := "b01".U // INCR

  val burst = RegInit(0.U(WordIndexBits.W))
  val dataLatch = RegInit(0.U(32.W))
  val rdata = io.axi.rdata

  io.metaArrayWrite.valid := false.B
  io.metaArrayWrite.index := DontCare
  io.metaArrayWrite.tag := DontCare
  val axiDataLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
  // FIXME: Data should be written to dataArray together
  when(io.axi.rvalid && state === s_wait_data) {
    burst := burst + 1.U
    axiDataLatch(burst) := rdata
  }
  when(io.axi.rlast && state === s_wait_data) {
    burst := 0.U
    // dataArray update 
    dataArray(index)(0)(0) := axiDataLatch(0)
    dataArray(index)(0)(1) := axiDataLatch(1)
    dataArray(index)(0)(2) := axiDataLatch(2)
    dataArray(index)(0)(3) := rdata
    // metaArray update
    io.metaArrayWrite.valid := true.B
    io.metaArrayWrite.index := index
    io.metaArrayWrite.tag := tag
  }
  
  when(burst === io.in.bits.wordIndex) {
    dataLatch := io.axi.rdata 
  }

  io.out.bits.rdata := DontCare
  when(!hit){
    io.out.bits.rdata := dataLatch
  }.otherwise{
    io.out.bits.rdata := cacheData
  }

  io.out.bits.addr := io.in.bits.addr
  io.out.valid := ((hit && state === s_idle && (!io.flush && io.in.valid)) || (state === s_valid && !refetch))
  io.in.ready := (!io.in.valid || io.out.fire) && (state === s_idle || (io.axi.rlast && state === s_wait_data))
}

class Stage3(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2Out))
    val out = Decoupled(new Stage3Out)
    val inFire = Input(Bool())
    val cacheDataVec = Input(Vec(LineBeats, UInt(32.W)))
    val flush = Input(Bool())
  })
  val cacheDataVec = io.cacheDataVec
  val wordIndex = io.in.bits.wordIndex
  val hit = io.in.bits.hit
  dontTouch(io.in.bits.hit)
  val flag = RegNext(io.inFire)
  val cacheDataChoosen = RegInit(0.U(32.W))
  when(flag){
    cacheDataChoosen := cacheDataVec(wordIndex)
  }
  val rdata = Mux(hit, Mux(flag, cacheDataVec(wordIndex), cacheDataChoosen), io.in.bits.rdata)
  dontTouch(rdata)

  io.out.bits.rdata := rdata
  io.out.bits.addr := io.in.bits.addr
  io.in.ready := !io.in.valid || io.out.fire
  io.out.valid := io.in.valid && !io.flush 
  // when(io.out.fire){ printf("pc = %x, inst = %x\n",io.in.bits.addr, io.in.bits.rdata) }
}

class PipelinedICache(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle{
    val in = new Stage1In
    val out = Decoupled(new Stage3Out)
    val axi = new AXI
    val s1Fire = Output(Bool())
    val flush = Input(Bool())
  })
  val s1 = Module(new Stage1)
  val s2 = Module(new Stage2)
  val s3 = Module(new Stage3)
  s1.io.flush := io.flush
  s2.io.flush := io.flush 
  s3.io.flush := io.flush

  s2.io.metaArrayTag := s1.io.metaArrayTag
  s3.io.inFire := s2.io.out.fire 
  for (i <- 0 until LineBeats) {
    s3.io.cacheDataVec(i) := s2.io.cacheDataVec(i)
  }

  s2.io.axi <> io.axi
  s1.io.in := io.in
  s1.io.metaArrayWrite <> s2.io.metaArrayWrite

  ICachePipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire, io.flush)
  ICachePipelineConnect(s2.io.out, s3.io.in, s3.io.out.fire, io.flush)
  s3.io.out <> io.out
  io.s1Fire := s1.io.out.fire
}

