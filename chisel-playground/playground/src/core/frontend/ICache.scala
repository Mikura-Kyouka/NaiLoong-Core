package core

import chisel3._
import chisel3.util._

case class CacheConfig(
    totalSize: Int = 4 * 16, // Bytes
    ways: Int = 1
)

sealed trait HasCacheConst {
  implicit val cacheConfig: CacheConfig

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

sealed abstract class CacheBundle(implicit cacheConfig: CacheConfig)
    extends Bundle
    with HasCacheConst

sealed abstract class CacheModule(implicit cacheConfig: CacheConfig)
    extends Module
    with HasCacheConst

sealed class MetaBundle(implicit val cacheConfig: CacheConfig)
    extends CacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(UInt(1.W))

  def apply(tag: UInt, valid: UInt) = {
    this.tag := tag
    this.valid := valid
    this
  }
}

sealed class DataBundle(implicit val cacheConfig: CacheConfig)
    extends CacheBundle {
  val data = Output(UInt(32.W)) // DataBits

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

class ICache(implicit val cacheConfig: CacheConfig) extends CacheModule {
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
  
  val metaArray = SyncReadMem(Sets, Vec(Ways, new MetaBundle))
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

  // icache tracer
  // val state0 = RegInit(s_idle) 
  // val state1 = RegInit(s_idle)
  // dontTouch(state0)
  // dontTouch(state1)
  // state0 := state1
  // state1 := state
  // val icacheTracer = Module(new ICacheTracer)
  // icacheTracer.io.access := state1 === s_idle && state === s_judge
  // icacheTracer.io.hit := state0 === s_idle && state1 === s_judge && state === s_data_access
  // icacheTracer.io.addr := io.addr
  // icacheTracer.io.clock := clock
  // icacheTracer.io.flag := state =/= s_idle && state =/= s_valid
}

// hit: idle -> judge -> data_access -> result_drive
// miss: idle -+> judge -+> fetching -> wait_data -+---------- -------------------+-> s_judge -> s_data_access -+> result_drive
//             |         |                         +-> s_fetching -> s_wait_data -+                             | 
//      S1     |  S2     |                          S3                                                          |      S4 

// Stage1: Address Calculation
class Stage1In extends Bundle {
  val addr = Input(UInt(32.W))
  val valid = Input(Bool())
}

class Stage1Out(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val addr = Output(UInt(32.W))
  val wordIndex = Output(UInt(WordIndexBits.W))
  val index = Output(UInt(IndexBits.W))
  val tag = Output(UInt(TagBits.W))
}

// Srage2: Tag Access
class Stage2Out(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val addr = Output(UInt(32.W))
  val hitVec = Output(UInt(Ways.W))
  val hit = Output(Bool())
  val wordIndex = Output(UInt(WordIndexBits.W))
  val index = Output(UInt(IndexBits.W))
  val tag = Output(UInt(TagBits.W))
}

// Stage3: Data Access
class Stage3Out(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val addr = Output(UInt(32.W)) // TODO: Debug Only
  val rdata = Output(UInt(32.W))
  val hit = Output(Bool())
  val wordIndex = Output(UInt(WordIndexBits.W))
}

// Stage4: Result Drive
class Stage4Out extends Bundle {
  val addr = Output(UInt(32.W))
  val rdata = Output(UInt(32.W))
}

class metaArrayWriteBundle(implicit val cacheConfig: CacheConfig) extends CacheBundle {
  val index = Output(UInt(IndexBits.W))
  val tag = Output(UInt(TagBits.W))
  val valid = Output(Bool())
}

class Stage1(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = new Stage1In
    val out = Decoupled(new Stage1Out)
    val flush = Input(Bool())
  })

  val addr = io.in.addr.asTypeOf(addrBundle)
  io.out.bits.addr := io.in.addr
  io.out.bits.wordIndex := addr.WordIndex 
  io.out.bits.index := addr.index 
  io.out.bits.tag := addr.tag
  io.out.valid := io.in.valid && !io.flush
}

class Stage2(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage1Out))
    val out = Decoupled(new Stage2Out)
    val metaArrayWrite = Flipped(new metaArrayWriteBundle)
    val flush = Input(Bool())
  })

  val metaArray = SyncReadMem(Sets, Vec(Ways, new MetaBundle))
  val index = io.in.bits.index
  val tag = io.in.bits.tag
  val hitVec = VecInit(
    metaArray(index).map(m => m.tag === tag && m.valid === 1.U)
  ).asUInt
  
  when(io.metaArrayWrite.valid) {
    metaArray(io.metaArrayWrite.index)(0).tag := io.metaArrayWrite.tag
    metaArray(io.metaArrayWrite.index)(0).valid := true.B
  }
  // val hitWay = Wire(UInt(Ways.W))
  // for (i <- 0 until Ways)
  //   when(hitVec(i)) {
  //     hitWay :=  i.U 
  //   }

  io.out.bits.hitVec := hitVec
  val hit = hitVec.orR
  io.out.bits.hit := hit
  dontTouch(io.out.bits.hit)
  dontTouch(hit)

  io.out.bits.wordIndex := io.in.bits.wordIndex
  io.out.bits.addr := io.in.bits.addr
  io.out.bits.index := io.in.bits.index
  io.out.bits.tag := io.in.bits.tag 

  io.in.ready := !io.in.valid || io.out.fire
  io.out.valid := io.in.valid && !io.flush
}

class Stage3(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage2Out))
    val hit = Input(Bool())
    val hitVec = Input(UInt(Ways.W))
    val out = Decoupled(new Stage3Out)
    val axi = new AXI
    val metaArrayWrite = new metaArrayWriteBundle
    val flush = Input(Bool())
    val cacheDataVec = Output(Vec(LineBeats, UInt(32.W)))
  })
  // val refetchLatch = RegInit(false.B)
  // when(io.flush && state =/= s_idle) {refetchLatch := true.B} // TODO 
  // when(io.axi.rlast) {refetchLatch := false.B}
  // val refetch = io.flush || refetchLatch
  
  io.axi := DontCare
  val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))

  val addr = io.in.bits.addr
  val wordIndex = io.in.bits.wordIndex
  val index = io.in.bits.index 
  val tag = io.in.bits.tag
  // direct input from metaArray
  val hitVec = io.hitVec 
  val hit = io.hit && io.in.valid // TODO

  // need a reg to store hit state in case it goes to fetching state
  val hitReg = RegInit(false.B)
  when(hit) { hitReg := true.B }
  when(io.out.fire) { hitReg := false.B }
  val HIT = hit // || hitReg // FIXME
  // 可以运行到pc = a0001144， 然后会因为流水线中的指令没能冲刷干净而执行 pc = 0xa000ee4 的指令
  // val HIT = hit || hitReg // FIXME 
  // pc = 0xf000000 会因为取出来的指令2不对 挂掉
  dontTouch(HIT)

  // io.out.bits.hit := hit // TODO
  io.out.bits.hit := HIT // TODO
  // io.out.bits.hit := io.in.bits.hit
  io.out.bits.wordIndex := wordIndex
  dontTouch(io.out.bits.hit)
  dontTouch(io.in.bits.hit)

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
  when(io.flush && (state =/= s_idle || !hit)) {refetchLatch := true.B} // TODO hit = io.hit &&io.in.valid 
  when(io.axi.rlast) {refetchLatch := false.B}
  val refetch = io.flush || refetchLatch

  state := MuxLookup(state, s_idle)(Seq(
    // s_idle -> Mux(!hit && io.in.valid, s_fetching, s_idle), 
    s_idle -> Mux(!HIT && io.in.valid, s_fetching, s_idle),
    s_fetching -> Mux(io.axi.arready, s_wait_data, s_fetching),
    s_wait_data -> Mux(io.axi.rlast, Mux(refetch, Mux(HIT, s_idle, s_fetching), s_idle), s_wait_data)
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
    // dataArray(index)(0)(burst) := rdata
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
    when(wordIndex === Fill(WordIndexBits, 1.U(1.W))) { 
      // needed data is in the last transfer, directly drive the result from axi signal
      io.out.bits.rdata := io.axi.rdata
    }.otherwise{
      io.out.bits.rdata := dataLatch
    }
  }.otherwise{
    io.out.bits.rdata := cacheData
  }

  io.out.bits.addr := io.in.bits.addr
  // io.out.valid := ((HIT && state === s_idle) || (io.axi.rlast && state === s_wait_data && !refetch)) // TODO
  // 1. 去掉后 会在 pc = 0x3000005c 跳转时 多执行 pc=0x30000068 的指令, 理想情况是0x0f000000
  // io.out.valid := ((HIT && state === s_idle && !io.flush ) || (io.axi.rlast && state === s_wait_data && !refetch)) // TODO
  // 2. 此处添加 !io.flush 会在pc = 0x30000054的时候无法跳转到正确的地址 :-(
  io.out.valid := ((HIT && state === s_idle && (!io.flush && io.in.valid)) || (io.axi.rlast && state === s_wait_data && !refetch)) // TODO
  // 
  io.out.valid := ((HIT && state === s_idle && !io.flush && io.in.valid) || (io.axi.rlast && state === s_wait_data && !refetch)) // TODO
  io.in.ready := (!io.in.valid || io.out.fire) && (state === s_idle || io.axi.rlast && state === s_wait_data)
}

class Stage4(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Stage3Out))
    val out = Decoupled(new Stage4Out)
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
  when(io.out.fire){
    printf("pc = %x, inst = %x\n",io.in.bits.addr, io.in.bits.rdata)
  }
}

class PipelinedICache(implicit val cacheConfig: CacheConfig) extends CacheModule {
  val io = IO(new Bundle{
    val in = new Stage1In
    val out = Decoupled(new Stage4Out)
    val axi = new AXI
    val s1Fire = Output(Bool())
    val flush = Input(Bool())
  })
  val s1 = Module(new Stage1)
  val s2 = Module(new Stage2)
  val s3 = Module(new Stage3)
  val s4 = Module(new Stage4)
  s1.io.flush := io.flush
  s2.io.flush := io.flush
  s3.io.flush := io.flush // refetch signal
  s4.io.flush := io.flush
  s3.io.hit := s2.io.out.bits.hit
  s3.io.hitVec := s2.io.out.bits.hitVec

  s4.io.inFire := s3.io.out.fire 
  for (i <- 0 until LineBeats) {
    s4.io.cacheDataVec(i) := s3.io.cacheDataVec(i)
  }

  s3.io.axi <> io.axi
  s1.io.in := io.in
  s2.io.metaArrayWrite <> s3.io.metaArrayWrite
  PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire, io.flush) 
  PipelineConnect(s2.io.out, s3.io.in, s3.io.out.fire, io.flush)
  PipelineConnect(s3.io.out, s4.io.in, s4.io.out.fire, io.flush)
  s4.io.out <> io.out
  io.s1Fire := s1.io.out.fire
}

class ICacheTracer extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val access = Input(UInt(1.W))
    val hit = Input(UInt(1.W))
    val flag = Input(UInt(1.W))
    val addr = Input(UInt(32.W))
  })
  addResource("/icache-tracer.sv");
}