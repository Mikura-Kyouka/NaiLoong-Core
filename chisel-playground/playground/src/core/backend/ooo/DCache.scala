package core

import chisel3._
import chisel3.util._

class reqBundle extends Bundle {
  val addr = Output(UInt(32.W))
  val size = Output(UInt(2.W))
  val wdata = Output(UInt(32.W))
  val wmask = Output(UInt(4.W))
  val cmd = Output(Bool()) // 0: read, 1: write
  val moqIdx = Output(UInt(3.W))
  val isMMIO = Input(Bool())
  val failsc = Input(Bool())
  val cacopOp = Input(UInt(2.W))
  val cacopEn = Input(Bool())
}

class respBundle extends Bundle {
  val rdata = Output(UInt(32.W))
  val resp = Output(Bool()) // 0: ready, 1: error
  val moqIdx = Output(UInt(3.W))
  val cmd = Output(Bool()) // 0: read, 1: write
}

case class DCacheConfig(
    totalSize: Int = 4 * 16, // Bytes
    ways: Int = 1
)

sealed trait HasCacheConst {
  implicit val cacheConfig: DCacheConfig

  val TotalSize = cacheConfig.totalSize
  val Ways = cacheConfig.ways
  val LineSize = 16 // byte
  val LineBeats = LineSize / 4 // DATA WIDTH 32
  val Sets = TotalSize / LineSize / Ways
  val OffsetBits = log2Up(LineSize)
  val IndexBits = log2Up(Sets)
  val WordIndexBits = if (LineBeats == 1) 0 else log2Up(LineBeats)
  val TagBits = 32 - OffsetBits - IndexBits
  val WayBits = math.max(1, log2Ceil(Ways))

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)
    val index = UInt(IndexBits.W)
    val WordIndex = UInt(WordIndexBits.W)
    val byteOffset = UInt(2.W)
  }

  def getMataIdx(addr: UInt) = addr.asTypeOf(addrBundle).index
  def getDataIdx(addr: UInt) = Cat(getMataIdx(addr), addr.asTypeOf(addrBundle).WordIndex)

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
}

sealed class MetaFlagBundle(implicit val cacheConfig: DCacheConfig)
    extends CacheBundle {
  val dirty = Output(Bool())
}

sealed class DataBundle(implicit val cacheConfig: DCacheConfig)
    extends CacheBundle {
  val data = Output(UInt(32.W))
}

class DCache(implicit val cacheConfig: DCacheConfig) extends CacheModule {
  require(Ways == 2, "DCache currently implements a two-way replacement policy")

  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new reqBundle))
    val resp = Decoupled(new respBundle)
    val axi = new AXI
    val flush = Input(Bool())
  })
  val reqReg = RegEnable(io.req.bits, io.req.fire)
  val req = Mux(io.req.fire, io.req.bits, reqReg)
  val addr = req.addr.asTypeOf(addrBundle)


  var single_workaround = req.addr(31, 24) =/= "h1c".U(8.W) && req.addr(31, 24) =/= "h1f".U(8.W) || true.B

  val cacopOp0 = req.cacopEn && req.cacopOp === CACOPOp.op0
  val cacopOp1 = req.cacopEn && req.cacopOp === CACOPOp.op1
  val cacopOp2 = req.cacopEn && req.cacopOp === CACOPOp.op2
  val isMMIO = req.isMMIO && !cacopOp1 && !cacopOp2

  val s_idle :: s_judge :: s_write_cache :: s_read_cache :: s_write_mem1 :: s_write_mem2 :: s_write_mem3 :: s_read_mem1 :: s_read_mem2 :: Nil = Enum(9)
  val state = RegInit(s_idle)

  // Each RAM word holds every way in one set.  A write must therefore merge the
  // selected way with the data read from the other way(s).
  val metaArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * TagBits))
  val metaValidArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways))
  val metaFlagArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways))
  val dataArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * LineBeats * 32))
  val replaceArray = Module(new DualPortBRAM(log2Ceil(Sets), 1))

  Seq(metaArray, metaValidArray, metaFlagArray, dataArray, replaceArray).foreach(_.io.clka := clock)
  metaArray.io.addra := addr.index
  metaValidArray.io.addra := addr.index
  metaFlagArray.io.addra := addr.index
  dataArray.io.addra := addr.index
  replaceArray.io.addra := addr.index
  metaArray.io.addrb := addr.index
  metaValidArray.io.addrb := addr.index
  metaFlagArray.io.addrb := addr.index
  dataArray.io.addrb := addr.index
  replaceArray.io.addrb := addr.index

  val metaReadData = metaArray.io.doutb.asTypeOf(Vec(Ways, new MetaBundle))
  val metaFlagData = metaFlagArray.io.doutb.asTypeOf(Vec(Ways, Bool()))
  val metaValidData = metaValidArray.io.doutb.asTypeOf(Vec(Ways, Bool()))
  val dataReadData = dataArray.io.doutb.asTypeOf(Vec(Ways, Vec(LineBeats, UInt(32.W))))

  val metaWriteData = Wire(Vec(Ways, UInt(TagBits.W)))
  val validWriteData = Wire(Vec(Ways, Bool()))
  val dirtyWriteData = Wire(Vec(Ways, Bool()))
  val dataWriteData = Wire(Vec(Ways, Vec(LineBeats, UInt(32.W))))
  for (way <- 0 until Ways) {
    metaWriteData(way) := metaReadData(way).tag
    validWriteData(way) := metaValidData(way)
    dirtyWriteData(way) := metaFlagData(way)
    dataWriteData(way) := dataReadData(way)
  }

  metaArray.io.dina := metaWriteData.asUInt
  metaValidArray.io.dina := validWriteData.asUInt
  metaFlagArray.io.dina := dirtyWriteData.asUInt
  dataArray.io.dina := dataWriteData.asUInt
  metaArray.io.wea := false.B
  metaValidArray.io.wea := false.B
  metaFlagArray.io.wea := false.B
  dataArray.io.wea := false.B
  replaceArray.io.dina := replaceArray.io.doutb
  replaceArray.io.wea := false.B

  val hitVec = VecInit((0 until Ways).map { way =>
    metaReadData(way).tag === addr.tag && metaValidData(way)
  })
  val hit = hitVec.asUInt.orR
  val hitWay = PriorityEncoder(hitVec.asUInt)
  val invalidVec = VecInit((0 until Ways).map(way => !metaValidData(way)))
  val victimWay = Mux(invalidVec.asUInt.orR, PriorityEncoder(invalidVec.asUInt), replaceArray.io.doutb)
  // Index CACOP encodes the way in the low address bits; the supervisor's
  // flush loop builds exactly this address with set_addr | way.
  val accessWay = Mux(cacopOp0 || cacopOp1, req.addr(WayBits - 1, 0), Mux(hit, hitWay, victimWay))
  val accessDirty = metaFlagData(accessWay)
  val selectedWay = RegInit(0.U(WayBits.W))
  when(state === s_judge) {
    selectedWay := accessWay
  }

  val flushed = RegInit(false.B)
  when(io.req.fire) { flushed := false.B }
  when(io.flush) { flushed := true.B }

  val wburst = RegInit(0.U(WordIndexBits.W))
  state := MuxLookup(state, s_idle)(Seq(
    s_idle -> Mux(io.req.fire && !cacopOp0 && !req.failsc &&
      !(single_workaround && !req.cacopEn),
      Mux(isMMIO, Mux(req.cmd, s_write_mem1, s_read_mem1), s_judge), s_idle),
    s_judge -> Mux(cacopOp1,
      Mux(accessDirty, s_write_mem1, s_idle),
      Mux(hit,
        Mux(cacopOp2, Mux(accessDirty, s_write_mem1, s_idle), Mux(req.cmd, s_write_cache, s_read_cache)),
        Mux(!cacopOp2, Mux(accessDirty, s_write_mem1, s_read_mem1), s_idle))),
    s_write_mem1 -> Mux(io.axi.awready, s_write_mem2, s_write_mem1),
    s_write_mem2 -> Mux(io.axi.wready && (wburst === (LineBeats - 1).U || isMMIO), s_write_mem3, s_write_mem2),
    s_write_mem3 -> Mux(io.axi.bvalid, Mux(isMMIO || cacopOp1 || cacopOp2, s_idle, s_read_mem1), s_write_mem3),
    s_read_mem1 -> Mux(io.axi.arready, s_read_mem2, s_read_mem1),
    s_read_mem2 -> Mux(io.axi.rvalid && io.axi.rlast, Mux(isMMIO, s_idle, Mux(req.cmd, s_write_cache, s_read_cache)), s_read_mem2),
    s_write_cache -> s_idle,
    s_read_cache -> s_idle
  ))

  io.req.ready := state === s_idle
  val resp = Wire(new respBundle)
  resp := DontCare
  resp.moqIdx := req.moqIdx
  resp.resp := false.B
  resp.rdata := 0.U
  resp.cmd := req.cmd
  io.resp.bits := resp
  io.resp.valid := (((isMMIO && (io.axi.rvalid || io.axi.bvalid)) ||
    ((cacopOp1 || cacopOp2) && state === s_write_mem3) ||
    (hit && cacopOp2 && !accessDirty && state === s_judge) ||
    (!hit && cacopOp2 && state === s_judge) ||
    (cacopOp1 && !accessDirty && state === s_judge)) && !flushed) ||
    (io.req.fire && cacopOp0 && state === s_idle) ||
    (io.req.fire && req.failsc && state === s_idle)

  io.axi := DontCare
  io.axi.arvalid := state === s_read_mem1
  io.axi.araddr := Mux(isMMIO, req.addr, Cat(addr.asUInt(31, OffsetBits), 0.U(OffsetBits.W)))
  io.axi.arid := 1.U
  io.axi.arlen := Mux(isMMIO, 0.U, (LineBeats - 1).U)
  io.axi.arsize := Mux(isMMIO, Cat(0.U(1.W), req.size), "b010".U)
  io.axi.arburst := "b01".U
  io.axi.rready := true.B

  val writebackData = dataReadData(selectedWay)(wburst)
  val writebackAddr = Cat(metaReadData(selectedWay).tag, addr.index, 0.U(OffsetBits.W))
  io.axi.awaddr := Mux(isMMIO, req.addr, writebackAddr)
  io.axi.awvalid := state === s_write_mem1
  io.axi.awid := 1.U
  io.axi.awlen := Mux(isMMIO, 0.U, (LineBeats - 1).U)
  io.axi.awsize := Mux(isMMIO, Cat(0.U(1.W), req.size), "b010".U)
  io.axi.awburst := "b01".U
  io.axi.wvalid := state === s_write_mem2
  io.axi.wlast := isMMIO || wburst === (LineBeats - 1).U
  io.axi.wid := 1.U
  io.axi.wstrb := Mux(isMMIO, req.wmask, "b1111".U)
  io.axi.wdata := Mux(isMMIO, req.wdata, writebackData)
  io.axi.bready := state === s_write_mem3

  val burst = RegInit(0.U(WordIndexBits.W))
  val axiDataLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
  when(io.axi.rvalid && state === s_read_mem2 && !isMMIO) {
    axiDataLatch(burst) := io.axi.rdata
    burst := burst + 1.U
  }

  val refillLine = Wire(Vec(LineBeats, UInt(32.W)))
  refillLine := axiDataLatch
  refillLine(burst) := io.axi.rdata
  val refill = io.axi.rvalid && io.axi.rlast && state === s_read_mem2 && !isMMIO
  when(refill) {
    burst := 0.U
    dataWriteData(selectedWay) := refillLine
    metaWriteData(selectedWay) := addr.tag
    validWriteData(selectedWay) := true.B
    dirtyWriteData(selectedWay) := false.B
    dataArray.io.wea := true.B
    metaArray.io.wea := true.B
    metaValidArray.io.wea := true.B
    metaFlagArray.io.wea := true.B
    replaceArray.io.wea := true.B
    replaceArray.io.dina := !selectedWay(0)
  }

  when(io.axi.wready && io.axi.wvalid && state === s_write_mem2 && !isMMIO) {
    wburst := wburst + 1.U
  }
  when(io.axi.bvalid) { wburst := 0.U }

  val originalLine = dataReadData(selectedWay)
  val origWord = originalLine(addr.WordIndex)
  val newWordBytes = Wire(Vec(4, UInt(8.W)))
  val origBytes = origWord.asTypeOf(Vec(4, UInt(8.W)))
  val wdataBytes = req.wdata.asTypeOf(Vec(4, UInt(8.W)))
  for (i <- 0 until 4) {
    newWordBytes(i) := Mux(req.wmask(i), wdataBytes(i), origBytes(i))
  }
  val updatedLine = Wire(Vec(LineBeats, UInt(32.W)))
  updatedLine := originalLine
  updatedLine(addr.WordIndex) := newWordBytes.asUInt

  when(state === s_write_cache) {
    dataWriteData(selectedWay) := updatedLine
    metaWriteData(selectedWay) := addr.tag
    validWriteData(selectedWay) := true.B
    dirtyWriteData(selectedWay) := true.B
    dataArray.io.wea := !isMMIO
    metaArray.io.wea := !isMMIO
    metaValidArray.io.wea := !isMMIO
    metaFlagArray.io.wea := !isMMIO
    replaceArray.io.wea := !isMMIO
    replaceArray.io.dina := !selectedWay(0)
    io.resp.valid := !flushed
  }

  when(state === s_read_cache) {
    resp.rdata := dataReadData(selectedWay)(addr.WordIndex)
    replaceArray.io.wea := !isMMIO
    replaceArray.io.dina := !selectedWay(0)
    io.resp.valid := !flushed
  }

  when(io.req.fire && single_workaround && !req.cacopEn) {
    io.resp.valid := !io.flush
  }
  when(isMMIO) { resp.rdata := io.axi.rdata }

  // CACOP op0 is immediate index invalidation.  op1/op2 invalidate only after
  // a dirty selected line has completed writeback, so the other way survives.
  when(io.req.fire && cacopOp0) {
    validWriteData(accessWay) := false.B
    dirtyWriteData(accessWay) := false.B
    metaValidArray.io.wea := true.B
    metaFlagArray.io.wea := true.B
  }
  val finishCacop = (cacopOp1 || (cacopOp2 && hit)) &&
    ((state === s_judge && !accessDirty) || (state === s_write_mem3 && io.axi.bvalid))
  val finishCacopWay = Mux(state === s_judge, accessWay, selectedWay)
  when(finishCacop) {
    validWriteData(finishCacopWay) := false.B
    dirtyWriteData(finishCacopWay) := false.B
    metaValidArray.io.wea := true.B
    metaFlagArray.io.wea := true.B
  }
}
