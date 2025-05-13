package core

import chisel3._
import chisel3.util._
import dataclass.data
import core.ALUOpType.add

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
  val LineSize = 4 // TODO: byte
  val LineBeats = LineSize / 4 // DATA WIDTH 32
  val Sets = TotalSize / LineSize / Ways
  val OffsetBits = log2Up(LineSize) // 26 6 2
  val IndexBits = log2Up(Sets)
  val WordIndexBits = if (LineBeats == 1) 0 else log2Up(LineBeats) // TODO
  val TagBits = 32 - OffsetBits - IndexBits

  // 打印参数
  println(s"TotalSize: $TotalSize, Ways: $Ways, LineSize: $LineSize, LineBeats: $LineBeats, Sets: $Sets, OffsetBits: $OffsetBits, IndexBits: $IndexBits, WordIndexBits: $WordIndexBits, TagBits: $TagBits")

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
        val req = Flipped(Decoupled(new reqBundle))
        val resp = Valid(new respBundle)
        val axi = new AXI
    })
    val req = io.req.bits
    val resp = io.resp.bits
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
    // reference: 《SuperScalar RISC Processor Design》 P. 103
    // hit -> write/read dataArray
    // !hit -> find one line -> dirty? --yes--> write this dirty cacheline to mem -> read from mem to cache -> read/write cache
    //                            +-------------------no----------------------------------+
    // NOTE: Write need to load first and then write, 
    // because store may only write to specific byte
    //   000        001          010          011               100               101            110          111
    val s_idle :: s_judge :: s_write_cache :: s_read_cache :: s_write_mem1 :: s_write_mem2 :: s_read_mem1 :: s_read_mem2 :: Nil = Enum(8)
    val state = RegInit(s_idle)
    state := MuxLookup(state, s_idle)(Seq(
        s_idle -> Mux(io.req.valid, s_judge, s_idle),
        s_judge -> Mux(hit, Mux(req.cmd, s_write_cache, s_read_cache), Mux(dirty, s_write_mem1, s_read_mem1)),
        s_write_mem1 -> Mux(io.axi.awready && io.axi.wready, s_write_mem2, s_write_mem1),
        s_write_mem2 -> Mux(io.axi.bvalid, s_idle, s_write_mem2),
        s_read_mem1 -> Mux(io.axi.arready, s_read_mem2, s_read_mem1),
        s_read_mem2 -> Mux(io.axi.rvalid, Mux(req.cmd, s_write_cache, s_read_cache), s_read_mem2), //FIXME
        s_write_cache -> s_idle,
        s_read_cache -> s_idle
    ))
    io.req.ready := state === s_idle
    io.resp.valid := false.B
    io.resp.bits.resp := false.B
    io.resp.bits.rdata := 0.U(32.W)
    io.axi := DontCare
    val cacheData = dataArray(addr.index)(0)(0)
    // axi read chanel
    io.axi.arvalid := state === s_read_mem1
    io.axi.araddr := req.addr
    io.axi.rready := true.B
    // axi write chanel
    io.axi.awvalid := state === s_write_mem1
    io.axi.wvalid := state === s_write_mem1
    io.axi.wdata := cacheData
    io.axi.bready := true.B
    // 从下级存储器读取Data Block到Cache刚刚被选定的line中 
    // 将这个cacheline标记为not dirty的状态
    val rdata = io.axi.rdata
    when(io.axi.rvalid && state === s_read_mem2) {
      dataArray(addr.index)(0)(0) := rdata // TODO

      metaArray(addr.index)(0).tag := addr.tag
      metaArray(addr.index)(0).valid := true.B
      metaArray(addr.index)(0).dirty := false.B
    }

    val offset = req.addr(1, 0) << 3
    // 将store指令携带的数据写道Cache中
    // 将这个Cache line标记为dirty状态
    when(state === s_write_cache) {
      // dataArray write
      when(req.wmask === "b1111".U) {
        dataArray(addr.index)(0)(0) := req.wdata
      }.elsewhen(req.wmask === "b0011".U) {
        when(req.addr(1, 0) === "b00".U) {
          dataArray(addr.index)(0)(0)(15, 0) := req.wdata(15, 0)
        }.elsewhen(req.addr(1, 0) === "b01".U) {
          dataArray(addr.index)(0)(0)(23, 8) := req.wdata(15, 0)
        }.elsewhen(req.addr(1, 0) === "b10".U) {
          dataArray(addr.index)(0)(0)(31, 16) := req.wdata(15, 0)
        }
      }.elsewhen(req.wmask === "b0001".U) {
        when(req.addr(1, 0) === "b00".U) {
          dataArray(addr.index)(0)(0)(7, 0)   := req.wdata(7, 0)
        }.elsewhen(req.addr(1, 0) === "b01".U) {
          dataArray(addr.index)(0)(0)(15, 8)  := req.wdata(7, 0)
        }.elsewhen(req.addr(1, 0) === "b10".U) {
          dataArray(addr.index)(0)(0)(23, 16) := req.wdata(7, 0)
        }.otherwise {
          dataArray(addr.index)(0)(0)(31, 24) := req.wdata(7, 0)
        }
      }

      // metaArray write
      metaArray(addr.index)(0).tag := addr.tag
      metaArray(addr.index)(0).valid := true.B
      metaArray(addr.index)(0).dirty := true.B
      io.resp.valid := true.B
    }

    // 将所需要的数据返回给load指令
    when(state === s_read_cache){
      resp.rdata := cacheData >> offset
      io.resp.valid := true.B
    }

    when(io.req.valid){
      printf("DCache: %x, %x, %x, %x\n", req.addr, req.wdata, addr.tag, addr.index)
    }
}