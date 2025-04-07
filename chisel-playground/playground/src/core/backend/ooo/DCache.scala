package core

import chisel3._
import chisel3.util._

class reqBundle extends Bundle{
    val addr  = Output(UInt(32.W))
    val wdata = Output(UInt(32.W))
    val wmask = Output(UInt(4.W))
    val cmd   = Output(Bool())// 0: read, 1: write
}

class respBundle extends Bundle{
    val rdata = Output(UInt(32.W))
    val resp = Output(Bool()) // 0: ready, 1: error
}

case class DCacheConfig(
    totalSize: Int = 4 * 16, // Bytes
    ways: Int = 1
)

sealed trait HasCacheConst {
  implicit val cacheConfig: DCacheConfig

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

sealed abstract class CacheBundle(implicit cacheConfig: DCacheConfig)
    extends Bundle
    with HasCacheConst

sealed abstract class CacheModule(implicit cacheConfig: DCacheConfig)
    extends Module
    with HasCacheConst

sealed class MetaBundle(implicit val cacheConfig: DCacheConfig)
    extends CacheBundle {
  val tag = Output(UInt(TagBits.W))
  val valid = Output(UInt(1.W))
  val dirty = Output(UInt(1.W))

  def apply(tag: UInt, valid: UInt) = {
    this.tag := tag
    this.valid := valid
    this
  }
}

sealed class DataBundle(implicit val cacheConfig: DCacheConfig)
    extends CacheBundle {
  val data = Output(UInt(32.W)) // DataBits

  def apply(data: UInt) = {
    this.data := data
    this
  }
}

class DCache(implicit val cacheConfig: DCacheConfig) extends CacheModule{
    val io = IO(new Bundle{
        val req = Flipped(Valid(new reqBundle))
        val resp = Valid(new respBundle)
        val axi = new AXI
    })
    io.axi := DontCare
    val req = io.req.bits
    val addr = req.addr.asTypeOf(addrBundle)

    val metaArray = SyncReadMem(Sets, Vec(Ways, new MetaBundle))
    val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))
    
    val hitVec = VecInit(
      metaArray(addr.index).map(m => m.tag === addr.tag && m.valid === 1.U)
    ).asUInt
    val hit = hitVec.orR
    val dirtyHitVec = VecInit(
      metaArray(addr.index).map(m => m.tag === addr.tag && m.valid === 1.U && m.dirty === 1.U)
    ).asUInt
    val dirty = dirtyHitVec.orR
    // hit -> write/read dataArray
    // !hit -> find one line -> dirty? -> write this dirty cacheline to mem -> read from mem to cache -> read/write cache
    //                            +------------------------------------------------+
    // NOTE: Write need to load first and then write, 
    // because store may only write to specific byte
    val s_idle :: s_judge :: s_write_cache :: s_read_cache :: s_write_mem1 :: s_write_mem2 :: s_read_mem1 :: s_read_mem2 :: Nil = Enum(8)
    val state = RegInit(s_idle)
    state := MuxLookup(state, s_idle)(Seq(
        s_idle -> Mux(io.req.valid, s_judge, s_idle),
        s_judge -> Mux(hit, Mux(req.cmd, s_write_cache, s_read_cache), Mux(dirty, s_write_mem1, s_read_mem1)),
        s_write_mem1 -> Mux(io.axi.awready && io.axi.wready, s_write_mem2, s_write_mem1),
        s_write_mem2 -> Mux(io.axi.bvalid, s_idle, s_write_mem2),
        s_read_mem1 -> Mux(io.axi.arready, s_write_mem2, s_read_mem1),
        s_read_mem2 -> Mux(io.axi.rvalid, Mux(req.cmd, s_write_cache, s_read_cache), s_read_mem2), //FIXME
        s_write_cache -> s_idle,
        s_read_cache -> s_idle
    ))
    io.resp.valid := false.B
    io.resp.bits.resp := false.B
    io.resp.bits.rdata := 0.U(32.W)
}

// class ICache(implicit val cacheConfig: DCacheConfig) extends CacheModule {
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
  
//   val metaArray = SyncReadMem(Sets, Vec(Ways, new MetaBundle))
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

//   // icache tracer
//   val state0 = RegInit(s_idle) 
//   val state1 = RegInit(s_idle)
//   dontTouch(state0)
//   dontTouch(state1)
//   state0 := state1
//   state1 := state
//   val icacheTracer = Module(new ICacheTracer)
//   icacheTracer.io.access := state1 === s_idle && state === s_judge
//   icacheTracer.io.hit := state0 === s_idle && state1 === s_judge && state === s_data_access
//   icacheTracer.io.addr := io.addr
//   icacheTracer.io.clock := clock
//   icacheTracer.io.flag := state =/= s_idle && state =/= s_valid
// }

// // hit: idle -> judge -> data_access -> result_drive
// // miss: idle -+> judge -+> fetching -> wait_data -+---------- -------------------+-> s_judge -> s_data_access -+> result_drive
// //             |         |                         +-> s_fetching -> s_wait_data -+                             | 
// //      S1     |  S2     |                          S3                                                          |      S4 

// // Stage1: Tag Access 
// class Stage1In extends Bundle {
//   val addr = Input(UInt(32.W))
//   val valid = Input(Bool())
// }

// class Stage1Out(implicit val cacheConfig: DCacheConfig) extends CacheBundle {
//   val addr = Output(UInt(32.W))
//   val wordIndex = Output(UInt(WordIndexBits.W))
//   val index = Output(UInt(IndexBits.W))
//   val tag = Output(UInt(TagBits.W))
// }

// // Stage2: Data Access
// class Stage2Out(implicit val cacheConfig: DCacheConfig) extends CacheBundle {
//   val addr = Output(UInt(32.W))
//   val rdata = Output(UInt(32.W))
//   val hit = Output(Bool())
//   val wordIndex = Output(UInt(WordIndexBits.W))
// }

// // Stage3: Result Drive
// class Stage3Out extends Bundle {
//   val addr = Output(UInt(32.W))
//   val rdata = Output(UInt(32.W))
// }

// class metaArrayWriteBundle(implicit val cacheConfig: DCacheConfig) extends CacheBundle {
//   val index = Output(UInt(IndexBits.W))
//   val tag = Output(UInt(TagBits.W))
//   val valid = Output(Bool())
// }

// class Stage1(implicit val cacheConfig: DCacheConfig) extends CacheModule {
//   val io = IO(new Bundle {
//     val in = new Stage1In // Flipped(Decoupled(new Stage1Out))
//     val out = Decoupled(new Stage1Out)
//     val metaArrayWrite = Flipped(new metaArrayWriteBundle)
//     val metaArrayTag = Output(UInt(TagBits.W))
//     val flush = Input(Bool())
//   })

//   val addr = io.in.addr.asTypeOf(addrBundle)
//   val metaArray = SyncReadMem(Sets, Vec(Ways, new MetaBundle))
//   val index = addr.index 
//   val tag = addr.tag 
  
//   io.metaArrayTag := metaArray(index)(0).tag
//   when(io.metaArrayWrite.valid) {
//     metaArray(io.metaArrayWrite.index)(0).tag := io.metaArrayWrite.tag
//     metaArray(io.metaArrayWrite.index)(0).valid := true.B
//   }

//   io.out.bits.wordIndex := addr.WordIndex
//   io.out.bits.addr := io.in.addr 
//   io.out.bits.index := index 
//   io.out.bits.tag := tag

//   io.out.valid := io.in.valid && !io.flush
// }

// class Stage2(implicit val cacheConfig: DCacheConfig) extends CacheModule {
//   val io = IO(new Bundle {
//     val in = Flipped(Decoupled(new Stage1Out))
//     val metaArrayTag = Input(UInt(TagBits.W))
//     val out = Decoupled(new Stage2Out)
//     val axi = new AXI
//     val metaArrayWrite = new metaArrayWriteBundle
//     val flush = Input(Bool())
//     val cacheDataVec = Output(Vec(LineBeats, UInt(32.W)))
//   })
//   io.axi := DontCare
//   val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))

//   val addr = io.in.bits.addr
//   val wordIndex = io.in.bits.wordIndex
//   val index = io.in.bits.index 
//   val tag = io.in.bits.tag

//   val hit = io.metaArrayTag === io.in.bits.tag && io.in.valid
//   dontTouch(hit)

//   io.out.bits.hit := hit
//   io.out.bits.wordIndex := wordIndex

//   val cacheData = Wire(UInt(32.W))
//   cacheData := dataArray(index)(0)(wordIndex)
//   dontTouch(cacheData)
//   val cacheDataVec = Wire(Vec(LineBeats, UInt(32.W)))
//   cacheDataVec := dataArray(index)(0)
//   dontTouch(cacheDataVec)
//   for (i <- 0 until LineBeats) {
//     io.cacheDataVec(i) := cacheDataVec(i)
//   }
//   io.out.bits.wordIndex := wordIndex

//   // miss access
//   //    00          01           10           
//   val s_idle :: s_fetching :: s_wait_data :: s_valid :: Nil = Enum(4)
//   val state = RegInit(s_idle)
//   val refetchLatch = RegInit(false.B)
//   when(io.flush && (state =/= s_idle || !hit)) { refetchLatch := true.B }
//   when(io.axi.rlast) {refetchLatch := false.B}
//   val refetch = io.flush || refetchLatch

//   state := MuxLookup(state, s_idle)(Seq(
//     s_idle -> Mux(!hit && io.in.valid, s_fetching, s_idle),
//     s_fetching -> Mux(io.axi.arready, s_wait_data, s_fetching),
//     s_wait_data -> Mux(io.axi.rlast, Mux(refetch, Mux(hit, s_valid, s_fetching), s_valid), s_wait_data),
//     s_valid -> s_idle
//   ))
//   // axi read signals
//   io.axi.arvalid := state === s_fetching
//   io.axi.araddr := Cat(addr(31, OffsetBits), Fill(OffsetBits, 0.U(1.W)))
//   io.axi.arsize := "b010".U // burst size
//   io.axi.rready := true.B
//   // burst signals
//   io.axi.arlen := (LineBeats - 1).asUInt // Burst_Length = AxLEN[7:0] + 1
//   io.axi.arburst := "b01".U // INCR

//   val burst = RegInit(0.U(WordIndexBits.W))
//   val dataLatch = RegInit(0.U(32.W))
//   val rdata = io.axi.rdata

//   io.metaArrayWrite.valid := false.B
//   io.metaArrayWrite.index := DontCare
//   io.metaArrayWrite.tag := DontCare
//   val axiDataLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
//   // FIXME: Data should be written to dataArray together
//   when(io.axi.rvalid && state === s_wait_data) {
//     burst := burst + 1.U
//     axiDataLatch(burst) := rdata
//   }
//   when(io.axi.rlast && state === s_wait_data) {
//     burst := 0.U
//     // dataArray update 
//     dataArray(index)(0)(0) := axiDataLatch(0)
//     dataArray(index)(0)(1) := axiDataLatch(1)
//     dataArray(index)(0)(2) := axiDataLatch(2)
//     dataArray(index)(0)(3) := rdata
//     // metaArray update
//     io.metaArrayWrite.valid := true.B
//     io.metaArrayWrite.index := index
//     io.metaArrayWrite.tag := tag
//   }
  
//   when(burst === io.in.bits.wordIndex) {
//     dataLatch := io.axi.rdata 
//   }

//   io.out.bits.rdata := DontCare
//   when(!hit){
//     io.out.bits.rdata := dataLatch
//   }.otherwise{
//     io.out.bits.rdata := cacheData
//   }

//   io.out.bits.addr := io.in.bits.addr
//   io.out.valid := ((hit && state === s_idle && (!io.flush && io.in.valid)) || (state === s_valid && !refetch))
//   io.in.ready := (!io.in.valid || io.out.fire) && (state === s_idle || (io.axi.rlast && state === s_wait_data))
// }

// class Stage3(implicit val cacheConfig: DCacheConfig) extends CacheModule {
//   val io = IO(new Bundle {
//     val in = Flipped(Decoupled(new Stage2Out))
//     val out = Decoupled(new Stage3Out)
//     val inFire = Input(Bool())
//     val cacheDataVec = Input(Vec(LineBeats, UInt(32.W)))
//     val flush = Input(Bool())
//   })
//   val cacheDataVec = io.cacheDataVec
//   val wordIndex = io.in.bits.wordIndex
//   val hit = io.in.bits.hit
//   dontTouch(io.in.bits.hit)
//   val flag = RegNext(io.inFire)
//   val cacheDataChoosen = RegInit(0.U(32.W))
//   when(flag){
//     cacheDataChoosen := cacheDataVec(wordIndex)
//   }
//   val rdata = Mux(hit, Mux(flag, cacheDataVec(wordIndex), cacheDataChoosen), io.in.bits.rdata)
//   dontTouch(rdata)

//   io.out.bits.rdata := rdata
//   io.out.bits.addr := io.in.bits.addr
//   io.in.ready := !io.in.valid || io.out.fire
//   io.out.valid := io.in.valid && !io.flush 
//   // when(io.out.fire){ printf("pc = %x, inst = %x\n",io.in.bits.addr, io.in.bits.rdata) }
// }

// class PipelinedICache(implicit val cacheConfig: DCacheConfig) extends CacheModule {
//   val io = IO(new Bundle{
//     val in = new Stage1In
//     val out = Decoupled(new Stage3Out)
//     val axi = new AXI
//     val s1Fire = Output(Bool())
//     val flush = Input(Bool())
//   })
//   val s1 = Module(new Stage1)
//   val s2 = Module(new Stage2)
//   val s3 = Module(new Stage3)
//   s1.io.flush := io.flush
//   s2.io.flush := io.flush 
//   s3.io.flush := io.flush

//   s2.io.metaArrayTag := s1.io.metaArrayTag
//   s3.io.inFire := s2.io.out.fire 
//   for (i <- 0 until LineBeats) {
//     s3.io.cacheDataVec(i) := s2.io.cacheDataVec(i)
//   }

//   s2.io.axi <> io.axi
//   s1.io.in := io.in
//   s1.io.metaArrayWrite <> s2.io.metaArrayWrite

//   PipelineConnect(s1.io.out, s2.io.in, s2.io.out.fire, io.flush)
//   PipelineConnect(s2.io.out, s3.io.in, s3.io.out.fire, io.flush)
//   s3.io.out <> io.out
//   io.s1Fire := s1.io.out.fire
// }

// class ICacheTracer extends BlackBox with HasBlackBoxResource {
//   val io = IO(new Bundle {
//     val clock = Input(Clock())
//     val access = Input(UInt(1.W))
//     val hit = Input(UInt(1.W))
//     val flag = Input(UInt(1.W))
//     val addr = Input(UInt(32.W))
//   })
//   addResource("/icache-tracer.sv");
// }

