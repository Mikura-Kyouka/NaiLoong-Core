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
  // println(s"TotalSize: $TotalSize, Ways: $Ways, LineSize: $LineSize, LineBeats: $LineBeats, Sets: $Sets, OffsetBits: $OffsetBits, IndexBits: $IndexBits, WordIndexBits: $WordIndexBits, TagBits: $TagBits")

  def addrBundle = new Bundle {
    val tag = UInt(TagBits.W)       // 26
    val index = UInt(IndexBits.W)   // 4
    val WordIndex = UInt(WordIndexBits.W)  // 0
    val byteOffset = UInt(2.W)      // 2
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
  val tag = Output(UInt(TagBits.W))  // 26
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
        val resp = Decoupled(new respBundle)
        val axi = new AXI
        val RobLsuIn  = Flipped(DecoupledIO())
        val RobLsuOut = DecoupledIO()
        val flush = Input(Bool())
    })
    val req = io.req.bits
    val resp = Wire(new respBundle)
    resp := DontCare
    val addr = req.addr.asTypeOf(addrBundle)

    val metaArray = SyncReadMem(Sets, Vec(Ways, new MetaBundle))
    val dataArray = SyncReadMem(Sets, Vec(Ways, Vec(LineBeats, UInt(32.W))))
    
    when(reset.asBool){
      for (i <- 0 until Sets) {
        for (j <- 0 until Ways) {
          metaArray(i)(j).valid := false.B
          metaArray(i)(j).dirty := false.B
          metaArray(i)(j).tag := 0.U
        }
      }
    }
    when(reset.asBool){
      for (i <- 0 until Sets) {
        for (j <- 0 until Ways) {
          dataArray(i)(j) := VecInit(Seq.fill(LineBeats)(0.U(32.W)))
        }
      }
    }

    val isMMIO = req.addr(31, 16) === "hbfaf".U

    val hitVec = VecInit(
      metaArray(addr.index).map(m => m.tag === addr.tag && m.valid === 1.U)
    ).asUInt
    val hit = hitVec.orR
    dontTouch(hit)

    val dirty = metaArray(addr.index)(0).dirty.asBool
    dontTouch(dirty)

    val flushed = RegInit(false.B) // 用于标记当前事务是否已经被flush过
    when(io.req.valid) {
      flushed := false.B
    }
    when(io.flush) {
      flushed := true.B
    }

    // reference: 《SuperScalar RISC Processor Design》 P. 103
    // hit -> write/read dataArray
    // !hit -> find one line -> dirty? --yes--> write this dirty cacheline to mem -> read from mem to cache -> read/write cache
    //                            +-------------------no----------------------------------+
    // NOTE: Write need to load first and then write, 
    // because store may only write to specific byte
    //   000        001          010          011               100               101            110          111
    val s_idle :: s_judge :: s_wait_rob :: s_write_cache :: s_read_cache :: s_write_mem1 :: s_write_mem2 :: s_write_mem3 :: s_read_mem1 :: s_read_mem2 :: Nil = Enum(10)
    val state = RegInit(s_idle)
    state := MuxLookup(state, s_idle)(Seq(
        s_idle -> Mux(io.flush, s_idle, Mux(io.req.valid, Mux(isMMIO, Mux(req.cmd, s_wait_rob, s_read_mem1), s_judge), s_idle)),
        s_judge -> Mux(io.flush, s_idle, Mux(hit, Mux(req.cmd, s_wait_rob, s_read_cache), Mux(req.cmd, s_wait_rob, Mux(dirty, s_write_mem1, s_read_mem1)))),
        s_wait_rob -> Mux(io.flush, s_idle, Mux(io.RobLsuIn.valid, Mux(isMMIO, s_write_mem1, Mux(hit, s_write_cache, Mux(dirty, s_write_mem1, s_read_mem1))), s_wait_rob)),
        s_write_mem1 -> Mux(io.axi.awready, s_write_mem2, s_write_mem1),
        s_write_mem2 -> Mux(io.axi.wready, s_write_mem3, s_write_mem2),
        s_write_mem3 -> Mux(io.axi.bvalid, Mux(isMMIO, s_idle, s_read_mem1), s_write_mem3),
        s_read_mem1 -> Mux(io.axi.arready, s_read_mem2, s_read_mem1),
        s_read_mem2 -> Mux(io.axi.rvalid, Mux(isMMIO, s_idle, Mux(req.cmd, s_write_cache, s_read_cache)), s_read_mem2), //FIXME
        s_write_cache -> s_idle,
        s_read_cache -> s_idle
    ))
    io.req.ready := (state === s_idle || state === s_write_cache || state === s_read_cache || (isMMIO && io.axi.rvalid) || (isMMIO && io.axi.bvalid)) && 
                    (!io.req.valid || io.resp.fire)
    io.resp.valid := ((isMMIO && io.axi.rvalid) || (isMMIO && io.axi.bvalid)) && !flushed
    io.resp.bits.resp := false.B
    io.resp.bits.rdata := 0.U(32.W)
    io.axi := DontCare
    // 通知 ROB: Store 指令已经退休
    io.RobLsuOut.valid := (state === s_write_mem3 && io.axi.bvalid) || state === s_write_cache
    io.RobLsuIn.ready := state === s_wait_rob

    val cacheData = dataArray(addr.index)(0)(0)
    // axi read chanel
    io.axi.arvalid := state === s_read_mem1
    io.axi.araddr := req.addr
    io.axi.arid := 1.U(4.W)
    io.axi.arlen := 0.U
    io.axi.arsize := "b010".U  // 32 bits
    io.axi.arburst := "b01".U
    io.axi.rready := true.B
    // axi write chanel
    val awaddr = Cat(metaArray(addr.index)(0).tag, addr.index, 0.U(2.W))
    io.axi.awaddr := Mux(isMMIO, req.addr, awaddr)
    io.axi.awvalid := state === s_write_mem1
    io.axi.awid := 1.U(4.W)
    io.axi.awlen := 0.U
    io.axi.awsize := "b010".U  // 32 bits
    io.axi.awburst := "b01".U
    io.axi.wvalid := state === s_write_mem2
    io.axi.wid := 1.U(4.W)
    io.axi.wstrb := "b1111".U
    io.axi.wdata := Mux(isMMIO, req.wdata, cacheData)
    io.axi.bready := state === s_write_mem3
    // 从下级存储器读取Data Block到Cache刚刚被选定的line中 
    // 将这个cacheline标记为not dirty的状态
    val rdata = io.axi.rdata
    when(io.axi.rvalid && state === s_read_mem2 && !isMMIO) {
      dataArray(addr.index)(0)(0) := rdata // TODO

      // val writeMeta = Vec(Ways, new MetaBundle)
      // for (i <- 0 until Ways) {
      //   writeMeta(i).tag := addr.tag
      //   writeMeta(i).valid := true.B
      //   writeMeta(i).dirty := false.B
      // }
      // metaArray.write(addr.index, writeMeta)

      metaArray(addr.index)(0).tag := addr.tag
      metaArray(addr.index)(0).valid := true.B
      metaArray(addr.index)(0).dirty := false.B
    }

    val offset = req.addr(1, 0) << 3
    // 读取当前字
    val origWord = dataArray(addr.index)(0)(0)

    // 新的数据寄存器
    val newWord = Wire(UInt(32.W))
    newWord := origWord // 默认保持原值
    dontTouch(newWord)

    when(state === s_write_cache) {
      when(req.wmask === "b1111".U) {
        newWord := req.wdata
      }.elsewhen(req.wmask === "b0011".U) {
        when(req.addr(1, 0) === "b00".U) {
          // 更新低半字
          newWord := Cat(origWord(31, 16), req.wdata(15, 0))
        }.elsewhen(req.addr(1, 0) === "b10".U) {
          // 更新高半字
          newWord := Cat(req.wdata(15, 0), origWord(15, 0))
        }
      }.elsewhen(req.wmask === "b0001".U) {
        when(req.addr(1, 0) === "b00".U) {
          newWord := Cat(origWord(31, 8), req.wdata(7, 0))
        }.elsewhen(req.addr(1, 0) === "b01".U) {
          newWord := Cat(origWord(31, 16), req.wdata(7, 0), origWord(7, 0))
        }.elsewhen(req.addr(1, 0) === "b10".U) {
          newWord := Cat(origWord(31, 24), req.wdata(7, 0), origWord(15, 0))
        }.otherwise {
          newWord := Cat(req.wdata(7, 0), origWord(23, 0))
        }
      }
      // 完整写入更新后的数据
      dataArray(addr.index)(0) := VecInit(Seq(newWord))
      
      // 同时更新metaArray
      // val writeMeta = Vec(Ways, new MetaBundle)
      // for (i <- 0 until Ways) {
      //   writeMeta(i).tag := addr.tag
      //   writeMeta(i).valid := true.B
      //   writeMeta(i).dirty := true.B
      // }
      // metaArray.write(addr.index, writeMeta)

      metaArray(addr.index)(0).tag := addr.tag
      metaArray(addr.index)(0).valid := true.B
      metaArray(addr.index)(0).dirty := true.B

      io.resp.valid := !flushed // 如果没有被flush过，则返回有效响应
    }

    // 将所需要的数据返回给load指令
    when(state === s_read_cache){
      resp.rdata := Mux(isMMIO, io.axi.rdata, cacheData >> offset)
      io.resp.valid := !flushed // 如果没有被flush过，则返回有效响应
    }

    io.resp.bits := resp

    // when(io.req.valid){
    //   printf("DCache: %x, %x, %x, %x\n", req.addr, req.wdata, addr.tag, addr.index)
    // }
}