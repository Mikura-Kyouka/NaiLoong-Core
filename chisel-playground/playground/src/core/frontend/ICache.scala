package core

import chisel3._
import chisel3.util._

import utils._

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
  val WordIndexBits = if (LineBeats == 1) 0 else log2Up(LineBeats) // TODO
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

    val reg = RegInit(0.U.asTypeOf(left.bits))
    when(left.valid && right.ready) {
      reg := left.bits
    }
    
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

// class ICache(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
//   val io = IO(new Bundle {
//     val axi = new AXI
//     val ready = Input(Bool())
//     val inst = Output(UInt(32.W))
//     val instValid = Output(Bool())
//     val outFire = Input(Bool())
//     val addr = Input(UInt(32.W))
//     val addrValid = Input(Bool())
//     val fenceI = Input(Bool())
//     val refetch = Input(Bool())
//   })
//   io.axi := DontCare // walk around
//   val addr = io.addr.asTypeOf(addrBundle)
  
//   val metaArray = SyncReadMem(Sets, Vec(Ways, new ICacheMetaBundle))
//   val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))

//   // see 《SuperScalar RISC Processor Design》 P24
//   // 000     001(Tag Access)  010          011          100(Data Access) 101(Result Drive) 110 
//   val s_idle :: s_judge :: s_fetching :: s_wait_data :: s_data_access :: s_valid :: s_error :: Nil = Enum(7)
//   val state = RegInit(s_idle)

//   val hitVec = VecInit(
//     metaArray(addr.index).map(m => m.tag === addr.tag && m.valid === 1.U)
//   ).asUInt
//   val hit = hitVec.orR 
//   val hitfake = hitVec.orR
//   dontTouch(hitfake)
//   dontTouch(hit)

//   val cacheData = Wire(UInt(32.W))
//   cacheData := DontCare
//   for (i <- 0 until Ways)
//     when(hitVec(i)) {
//       cacheData := dataArray(addr.index)(i)(addr.WordIndex) // TODO
//     }
//   io.inst := cacheData

//   val refetchLatch = RegInit(false.B)
//   when(io.refetch) {refetchLatch := true.B} 
//   when(io.axi.rlast) {refetchLatch := false.B}
//   val refetch = io.refetch || refetchLatch

//   // hit: idle -> judge -> data_access -> result_drive
//   // miss: idle -+> judge -+> fetching -> wait_data -+---------- -------------------+-> s_judge -> s_data_access -+> result_drive
//   //             |         |                         +-> s_fetching -> s_wait_data -+                             | 
//   //      S1     |  S2     |                          S3                                                          |      S4 
//   state := MuxLookup(state, s_idle)(
//     Seq(
//       s_idle -> Mux(io.addrValid, s_judge, s_idle),
//       s_judge -> Mux(hit && ~refetch, s_data_access, s_fetching),
//       s_fetching -> Mux(io.axi.arready, s_wait_data, s_fetching),
//       s_wait_data -> Mux(io.axi.rlast, Mux(refetch, s_fetching, s_judge), s_wait_data),
//       s_data_access -> s_valid,
//       s_valid -> Mux(io.outFire, s_idle, s_valid) // Didn't stay
//     )
//   )

//   // axi read signals
//   io.axi.arvalid := state === s_fetching
//   io.axi.araddr := Cat(io.addr(31, OffsetBits), Fill(OffsetBits, 0.U(1.W)))
//   io.axi.arsize := "b010".U // burst size
//   io.axi.rready := true.B
//   // burst signals
//   io.axi.arlen := (LineBeats - 1).asUInt // Burst_Length = AxLEN[7:0] + 1
//   io.axi.arburst := "b01".U // INCR

//   // save addr info for burst transation
//   val burst = RegInit(0.U(WordIndexBits.W))

//   io.instValid := state === s_valid
//   // miss update
//   val rdata = io.axi.rdata
//   // rvalid in burst transaction
//   when(io.axi.rvalid && state === s_wait_data) {
//     burst := burst + 1.U
//     dataArray(addr.index)(0)(burst) := rdata // TODO
//   }
//   when(io.axi.rlast && state === s_wait_data) {
//     burst := 0.U
//     metaArray(addr.index)(0).tag := addr.tag
//     metaArray(addr.index)(0).valid := true.B
//   }

//   when(io.fenceI) {
//     for (i <- 0 until Sets)
//       for (j <- 0 until Ways)
//         metaArray(i)(j).valid := false.B
//   }
// }

// hit: idle -> judge -> data_access -> result_drive
// miss: idle -+> judge -+> fetching -> wait_data -+---------- -------------------+-> s_judge -> s_data_access -+> result_drive
//             |         |                         +-> s_fetching -> s_wait_data -+                             | 
//      S1     |  S2     |                          S3                                                          |      S4 

// Stage1: Tag Access 
class Stage1In extends Bundle {
  val addr = Input(UInt(32.W))
  val pc = Input(UInt(32.W))
  val mat = Input(UInt(2.W))
  val brPredictTaken = Input(Vec(4, new RedirectIO))
  val valid = Input(Bool())
}

class Stage1Out(implicit val cacheConfig: ICacheConfig) extends ICacheBundle {
  val addr = Output(UInt(32.W))
  val pc = Output(UInt(32.W))
  val mat = Output(UInt(2.W))
  val wordIndex = Output(UInt(WordIndexBits.W))
  val index = Output(UInt(IndexBits.W))
  val tag = Output(UInt(TagBits.W))
  val brPredictTaken = Output(Vec(4, new RedirectIO))
}

// Stage2: Data Access
class Stage2Out(implicit val cacheConfig: ICacheConfig) extends ICacheBundle {
  val addr = Output(UInt(32.W))
  val pc = Output(UInt(32.W))
  val rdata = Output(Vec(4, UInt(32.W)))
  val hit = Output(Bool())
  val wordIndex = Output(UInt(WordIndexBits.W))
  val brPredictTaken = Output(Vec(4, new RedirectIO))
}

// Stage3: Result Drive
class Stage3Out extends Bundle {
  val addr = Output(UInt(32.W))
  val pc = Output(UInt(32.W))
  val rdata = Output(UInt(32.W))
  val brPredictTaken = Output(Vec(4, new RedirectIO))
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
    val metaArrayValid = Output(Bool())
    val flush = Input(Bool())
  })

  val addr = io.in.addr.asTypeOf(addrBundle)
  val index = addr.index 
  val tag = addr.tag 

  // val metaArray = SyncReadMem(Sets, Vec(Ways, new ICacheMetaBundle))
  val metaArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * (TagBits))) // Ways * (TagBits)
  val metaValidArray = RegInit(VecInit(Seq.fill(Sets)(VecInit(Seq.fill(Ways)(false.B)))))
  val syncReadAddr = RegInit(0.U(log2Ceil(Sets).W))
  val collison_data = RegInit(0.U.asTypeOf(VecInit(Seq.fill(Ways)(false.B))))
  val is_collision = RegInit(false.B)
  is_collision := io.metaArrayWrite.index === index && io.metaArrayWrite.valid
  syncReadAddr := index
  collison_data := VecInit(Seq.fill(Ways)(true.B))

  // a 口只用于写入，b 口只用于读取
  metaArray.io.clka := clock
  metaArray.io.wea := io.metaArrayWrite.valid
  metaArray.io.addra := io.metaArrayWrite.index
  metaArray.io.dina := Cat(io.metaArrayWrite.tag)
  metaArray.io.addrb := index

  when(io.metaArrayWrite.valid) {
    metaValidArray(io.metaArrayWrite.index)(0) := true.B
  }
  
  val metaArrayInfo = metaArray.io.doutb
  io.metaArrayTag := metaArrayInfo
  io.metaArrayValid := Mux(is_collision, collison_data(0), metaValidArray(syncReadAddr)(0))

  io.out.bits.wordIndex := addr.WordIndex
  io.out.bits.addr := io.in.addr 
  io.out.bits.pc := io.in.pc
  io.out.bits.mat := io.in.mat
  io.out.bits.index := index 
  io.out.bits.tag := tag
  io.out.bits.brPredictTaken := io.in.brPredictTaken

  io.out.valid := io.in.valid && !io.flush
}

class Stage2(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1Out))
    val metaArrayTag = Input(UInt(TagBits.W))
    val metaArrayValid = Input(Bool())
    val out = Decoupled(new Stage2Out)
    val axi = new AXI
    val metaArrayWrite = new metaArrayWriteBundle
    val flush = Input(Bool())
    val cacheDataVec = Output(Vec(LineBeats, UInt(32.W)))
    val inFire = Input(Bool())
  })
  val addr = io.in.bits.addr
  val wordIndex = io.in.bits.wordIndex
  val index = io.in.bits.index 
  val tag = io.in.bits.tag

  io.axi := DontCare
  // val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))
  val dataArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * LineBeats * 32))

  dataArray.io.clka := clock
  dataArray.io.wea := false.B // TODO: see below
  dataArray.io.addra := index
  dataArray.io.dina := DontCare
  dataArray.io.addrb := index

  val HIT = io.metaArrayTag === io.in.bits.tag && io.metaArrayValid && io.in.valid
  dontTouch(HIT)
  val hitEn = RegNext(io.inFire)
  val hitReg = RegEnable(HIT, hitEn) // RegEnable(nextValue, enable)
  val hit = HIT || (hitReg && ~hitEn)
  dontTouch(hit)

  io.out.bits.hit := hit
  io.out.bits.wordIndex := wordIndex
  io.out.bits.brPredictTaken := io.in.bits.brPredictTaken
  io.out.bits.pc := io.in.bits.pc

  val cacheData = Wire(Vec(LineBeats, UInt(32.W)))
  cacheData := dataArray.io.doutb.asTypeOf(Vec(LineBeats, UInt(32.W)))
  dontTouch(cacheData)
  val cacheDataVec = Wire(Vec(LineBeats, UInt(32.W)))
  cacheDataVec := dataArray.io.doutb.asTypeOf(Vec(LineBeats, UInt(32.W)))
  dontTouch(cacheDataVec)
  for (i <- 0 until LineBeats) {
    io.cacheDataVec(i) := cacheDataVec(i)
  }
  io.out.bits.wordIndex := wordIndex

  // miss access
  //    00          01           10           
  val s_idle :: s_fetching :: s_wait_data :: s_valid :: s_judge :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val refetchLatch = RegInit(false.B)
  when(io.flush && (state =/= s_idle || !hit)) { refetchLatch := true.B }
  when(io.axi.rlast && io.axi.rvalid) {refetchLatch := false.B}
  val refetch = io.flush || refetchLatch

  val FLAG = io.axi.rlast && io.axi.rvalid
  dontTouch(FLAG)

  state := MuxLookup(state, s_idle)(Seq(
    s_idle -> Mux(!hit && io.in.valid, s_fetching, s_idle),
    s_fetching -> Mux(io.axi.arready, s_wait_data, s_fetching),
    s_wait_data -> Mux(io.axi.rlast && io.axi.rvalid, Mux(refetch, s_idle, s_valid), s_wait_data),
    // s_valid -> Mux(!io.axi.rlast, s_idle, s_valid)
    s_valid -> s_idle
  ))

  // 命中率统计
  val hitCount = RegInit(0.U(32.W))
  val accessCount = RegInit(0.U(32.W))
  dontTouch(hitCount)
  dontTouch(accessCount)
  when(hitEn) { 
    // printf("addr = %x\n", io.in.bits.addr)
    when(hit) {
      hitCount := hitCount + 1.U
    }
    accessCount := accessCount + 1.U
  }

  // axi read signals
  io.axi.arvalid := state === s_fetching
  io.axi.araddr := Cat(addr(31, OffsetBits), Fill(OffsetBits, 0.U(1.W)))
  io.axi.arsize := "b010".U // burst size
  io.axi.rready := true.B
  // burst signals
  io.axi.arlen := (LineBeats - 1).asUInt // Burst_Length = AxLEN[7:0] + 1
  io.axi.arburst := "b01".U // INCR

  val burst = RegInit(0.U(WordIndexBits.W))
  val dataLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
  val rdata = io.axi.rdata

  io.metaArrayWrite.valid := false.B
  io.metaArrayWrite.index := DontCare
  io.metaArrayWrite.tag := DontCare
  val axiDataLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
  // FIXME: Data should be written to dataArray together
  when(io.axi.rvalid && state === s_wait_data) {
    burst := burst + 1.U
    axiDataLatch(burst) := rdata
    dataLatch(burst) := rdata
  }
  when((io.axi.rlast && io.axi.rvalid) && state === s_wait_data) {
    burst := 0.U
    // dataArray update 
    // dataArray(index)(0)(0) := axiDataLatch(0)
    // dataArray(index)(0)(1) := axiDataLatch(1)
    // dataArray(index)(0)(2) := axiDataLatch(2)
    // dataArray(index)(0)(3) := rdata
    dataArray.io.wea := true.B
    dataArray.io.dina := Cat(rdata, axiDataLatch(2), axiDataLatch(1), axiDataLatch(0))
    // metaArray update
    io.metaArrayWrite.valid := io.in.bits.mat === 1.U // 一致可缓存
    io.metaArrayWrite.index := index
    io.metaArrayWrite.tag := tag
  }
  
  // when(burst === io.in.bits.wordIndex) {
  //   dataLatch := io.axi.rdata 
  // }

  // io.out.bits.rdata := DontCare
  when(!hit){
    io.out.bits.rdata := dataLatch
  }.otherwise{
    io.out.bits.rdata := cacheData
  }

  io.out.bits.addr := io.in.bits.addr
  io.out.valid := ((hit && state === s_idle && (!io.flush && io.in.valid)) || (state === s_valid && !refetch))
  io.in.ready := (!io.in.valid || io.out.fire) && (state === s_idle || ((io.axi.rlast && io.axi.rvalid) && state === s_wait_data))
}

class Stage3(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2Out))
    // val out = Decoupled(new Stage3Out)
    val out = Decoupled(Vec(4, new IFU2IDU))
    val inFire = Input(Bool())
    val cacheDataVec = Input(Vec(LineBeats, UInt(32.W)))
    val flush = Input(Bool())
  })
  val cacheDataVec = io.cacheDataVec
  val wordIndex = io.in.bits.wordIndex
  val hit = io.in.bits.hit
  dontTouch(io.in.bits.hit)
  val flag = RegNext(io.inFire)
  dontTouch(flag)
  val cacheDataVecLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
  when(flag){
    cacheDataVecLatch := cacheDataVec
  }
  val rdata = Mux(hit, Mux(flag, cacheDataVec, cacheDataVecLatch), io.in.bits.rdata)
  dontTouch(rdata)

  val ValidVec = Wire(UInt(4.W))
  ValidVec := MuxLookup(io.in.bits.addr(3,2), 0.U(4.W))(
    Seq(
      0.U -> "b1111".U,
      1.U -> "b1110".U,
      2.U -> "b1100".U,
      3.U -> "b1000".U
    )
  )

  // 0 0000, 4 0100, 8 1000, c 1100
  io.out.bits(0).inst := rdata(0)
  io.out.bits(0).pc := Cat(io.in.bits.pc(31, 4), "h0".U(4.W))
  io.out.bits(0).Valid := ValidVec(0)
  io.out.bits(0).brPredict := io.in.bits.brPredictTaken(0)

  io.out.bits(1).inst := rdata(1)
  io.out.bits(1).pc := Cat(io.in.bits.pc(31, 4), "h4".U(4.W))
  io.out.bits(1).Valid := ValidVec(1)
  io.out.bits(1).brPredict := io.in.bits.brPredictTaken(1)

  io.out.bits(2).inst := rdata(2)
  io.out.bits(2).pc := Cat(io.in.bits.pc(31, 4), "h8".U(4.W))
  io.out.bits(2).Valid := ValidVec(2)
  io.out.bits(2).brPredict := io.in.bits.brPredictTaken(2)

  io.out.bits(3).inst := rdata(3)
  io.out.bits(3).pc := Cat(io.in.bits.pc(31, 4), "hc".U(4.W))
  io.out.bits(3).Valid := ValidVec(3)
  io.out.bits(3).brPredict := io.in.bits.brPredictTaken(3)

  io.in.ready := !io.in.valid || io.out.fire
  io.out.valid := io.in.valid && !io.flush 

  io.out.bits(0).excp := DontCare
  io.out.bits(1).excp := DontCare
  io.out.bits(2).excp := DontCare
  io.out.bits(3).excp := DontCare
  // when(io.out.fire){ printf("pc = %x, inst = %x\n",io.in.bits.addr, io.in.bits.rdata) }
}

class PipelinedICache(implicit val cacheConfig: ICacheConfig) extends ICacheModule {
  val io = IO(new Bundle{
    val in = new Stage1In
    val out = Decoupled(Vec(4, new IFU2IDU))
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
  s2.io.metaArrayValid := s1.io.metaArrayValid
  s2.io.inFire := s1.io.out.fire
  s3.io.inFire := s2.io.out.fire
  for (i <- 0 until LineBeats) {
    s3.io.cacheDataVec(i) := s2.io.cacheDataVec(i)
  }

  s2.io.axi <> io.axi
  s1.io.in := io.in
  s1.io.metaArrayWrite <> s2.io.metaArrayWrite

  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire, io.flush)
  PipelineConnect(s2.io.out, s3.io.in, s3.io.out.fire, io.flush)
  s3.io.out <> io.out
  io.s1Fire := s1.io.out.fire
}

