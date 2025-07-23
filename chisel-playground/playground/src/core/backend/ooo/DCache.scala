package core

import chisel3._
import chisel3.util._
import core.ALUOpType.add

class reqBundle extends Bundle{
    val addr  = Output(UInt(32.W))
    val size  = Output(UInt(2.W))
    val wdata = Output(UInt(32.W))
    val wmask = Output(UInt(4.W))
    val cmd   = Output(Bool())// 0: read, 1: write
    val moqIdx = Output(UInt(3.W)) // moq entry index
    val isMMIO = Input(Bool())
}

class respBundle extends Bundle{
    val rdata = Output(UInt(32.W))
    val resp = Output(Bool()) // 0: ready, 1: error
    val moqIdx = Output(UInt(3.W)) // moq entry index
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
  // val dirty = Output(UInt(1.W))

  def apply(tag: UInt) = {
    this.tag := tag
    this
  }
}

sealed class MetaFlagBundle(implicit val cacheConfig: DCacheConfig)
    extends CacheBundle {
  // val tag = Output(UInt(TagBits.W))  // 26
  val dirty = Output(Bool()) // 1

  def apply(dirty: Bool) = {
    // this.tag := tag
    this.dirty := dirty
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
        val flush = Input(Bool())
        val cacop = Input(new CACOPIO)
    })
    val cacopOp0 = io.cacop.en && io.cacop.op === CACOPOp.op0
    val cacopOp1 = io.cacop.en && io.cacop.op === CACOPOp.op1
    val cacopOp2 = io.cacop.en && io.cacop.op === CACOPOp.op2

    val reqReg = RegEnable(io.req.bits, io.req.fire)
    val req = Mux(io.req.fire, io.req.bits, reqReg)

    val resp = Wire(new respBundle)
    resp := DontCare
    resp.moqIdx := req.moqIdx // 保留moq entry index
    val addr = req.addr.asTypeOf(addrBundle)
    when(cacopOp0 || cacopOp1) {
      addr := io.cacop.VA.asTypeOf(addrBundle)
    }

    val isMMIO = req.isMMIO //FIXME：
    // val isMMIO = false.B

    //    0          1              2               3             4                  5               6               7               8
    val s_idle :: s_judge :: s_write_cache :: s_read_cache :: s_write_mem1 :: s_write_mem2 :: s_write_mem3 :: s_read_mem1 :: s_read_mem2 :: Nil = Enum(9)

    val state = RegInit(s_idle)

    // 暂时只支持 1 way
    val metaArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * (TagBits)))
    val metaValidArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * 1))
    val metaFlagArray = RegInit(VecInit(Seq.fill(Sets)(VecInit(Seq.fill(Ways)(0.U.asTypeOf(new MetaFlagBundle))))))
    val dataArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * LineBeats * 32))

    // a 口只用于写入，b 口只用于读取
    metaArray.io.clka := clock
    metaArray.io.addra := addr.index
    metaArray.io.dina := 0.U // 后续覆盖
    metaArray.io.wea := false.B // 后续覆盖
    metaArray.io.addrb := addr.index

    metaValidArray.io.clka := clock
    metaValidArray.io.addra := addr.index
    metaValidArray.io.dina := 0.U // 后续覆盖
    metaValidArray.io.wea := false.B // 后续覆盖
    metaValidArray.io.addrb := addr.index

    dataArray.io.clka := clock
    dataArray.io.addra := addr.index
    dataArray.io.dina := 0.U // 后续覆盖
    dataArray.io.wea := false.B // 后续覆盖
    dataArray.io.addrb := addr.index

    val metaReadData = metaArray.io.doutb.asTypeOf(Vec(Ways, new MetaBundle))
    val syncReadAddr = RegInit(0.U(log2Ceil(Sets).W))
    val is_collision = RegInit(false.B)
    val collison_data = RegInit(0.U.asTypeOf(VecInit(Seq.fill(Ways)(0.U.asTypeOf(new MetaFlagBundle)))))
    syncReadAddr := addr.index
    is_collision := io.axi.rvalid && state === s_read_mem2 && !isMMIO || state === s_write_cache
    collison_data := Mux(io.axi.rvalid && state === s_read_mem2 && !isMMIO, VecInit(Seq.fill(Ways)(false.B.asTypeOf(new MetaFlagBundle))), 
                                                                            VecInit(Seq.fill(Ways)(true.B.asTypeOf(new MetaFlagBundle))))

    val metaFlagData = Mux(is_collision, collison_data, metaFlagArray(syncReadAddr))
    val metaValidData = metaValidArray.io.doutb.asTypeOf(Vec(Ways, Bool()))
    val dataReadData = dataArray.io.doutb.asTypeOf(Vec(Ways, Vec(LineBeats, UInt(32.W))))

    val hitVec = VecInit(
      metaReadData.map(m => m.tag === addr.tag && metaValidData(0))
    ).asUInt

    val hit = hitVec.orR

    val dirty = metaFlagData(0).dirty

    val flushed = RegInit(false.B) // 用于标记当前事务是否已经被flush过
    when(io.req.fire) {
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

    state := MuxLookup(state, s_idle)(Seq(
        s_idle -> Mux(io.req.fire && !cacopOp0, 
                        Mux(cacopOp1, Mux(dirty, s_write_mem1, s_idle), Mux(isMMIO, Mux(req.cmd, s_write_mem1, s_read_mem1), s_judge)), 
                        s_idle),
        s_judge -> Mux(hit, Mux(cacopOp2, s_idle, Mux(req.cmd, s_write_cache, s_read_cache)), 
                            Mux(!cacopOp2 && req.cmd, Mux(dirty, s_write_mem1, s_read_mem1), Mux(dirty, s_write_mem1, s_read_mem1))),
        s_write_mem1 -> Mux(io.axi.awready, s_write_mem2, s_write_mem1),
        s_write_mem2 -> Mux(io.axi.wready, s_write_mem3, s_write_mem2),
        s_write_mem3 -> Mux(io.axi.bvalid, Mux(isMMIO || cacopOp1, s_idle, s_read_mem1), s_write_mem3),
        s_read_mem1 -> Mux(io.axi.arready, s_read_mem2, s_read_mem1),
        s_read_mem2 -> Mux(io.axi.rvalid, Mux(isMMIO, s_idle, Mux(req.cmd, s_write_cache, s_read_cache)), s_read_mem2), // FIXME:
        s_write_cache -> s_idle,
        s_read_cache -> s_idle
    ))

    io.req.ready := state === s_idle
    io.resp.valid := ((isMMIO && io.axi.rvalid) || (isMMIO && io.axi.bvalid) || 
                     (io.req.valid && cacopOp0 && state === s_idle)) && !flushed 
    io.resp.bits.resp := false.B
    io.resp.bits.rdata := 0.U(32.W)
    io.axi := DontCare

    when(cacopOp0) {
      metaArray.io.wea := true.B
      metaArray.io.addra := addr.index
      metaArray.io.dina := 0.U
    }

    // val offset = req.addr(1, 0) << 3
    val cacheData = dataReadData(0)(0)
    // axi read chanel
    io.axi.arvalid := state === s_read_mem1
    io.axi.araddr := addr.asUInt
    io.axi.arid := 1.U(4.W)
    io.axi.arlen := 0.U
    io.axi.arsize := "b010".U  // 32 bits
    io.axi.arburst := "b01".U
    io.axi.rready := true.B
    // axi write chanel
    val awaddr = Cat(metaReadData(0).tag, addr.index, 0.U(2.W))
    io.axi.awaddr := Mux(isMMIO, req.addr, awaddr)
    io.axi.awvalid := state === s_write_mem1
    io.axi.awid := 1.U(4.W)
    io.axi.awlen := 0.U
    io.axi.awsize := "b010".U  // 32 bits
    io.axi.awburst := "b01".U
    io.axi.wvalid := state === s_write_mem2
    io.axi.wid := 1.U(4.W)
    io.axi.wstrb := Mux(isMMIO, req.wmask, "b1111".U) 
    io.axi.wdata := Mux(isMMIO, req.wdata, cacheData)
    io.axi.bready := state === s_write_mem3
    // 从下级存储器读取Data Block到Cache刚刚被选定的line中 
    // 将这个cacheline标记为not dirty的状态
    val rdata = io.axi.rdata
    when(io.axi.rvalid && state === s_read_mem2 && !isMMIO) {
      dataArray.io.wea := !isMMIO // 一致可缓存
      dataArray.io.addra := addr.index
      dataArray.io.dina := VecInit(Seq.fill(LineBeats)(rdata)).asUInt
      // dataArray(addr.index)(0)(0) := rdata // TODO

      // val writeMeta = Vec(Ways, new MetaBundle)
      // for (i <- 0 until Ways) {
      //   writeMeta(i).tag := addr.tag
      //   writeMeta(i).valid := true.B
      //   writeMeta(i).dirty := false.B
      // }
      // metaArray.write(addr.index, writeMeta)

      // metaArray(addr.index)(0).tag := addr.tag
      // metaArray(addr.index)(0).valid := true.B
      // metaArray(addr.index)(0).dirty := false.B
      metaArray.io.wea := !isMMIO // 一致可缓存
      metaArray.io.addra := addr.index
      metaArray.io.dina := addr.tag // tag
      metaValidArray.io.wea := !isMMIO // 一致可缓存
      metaValidArray.io.addra := addr.index
      metaValidArray.io.dina := true.B // valid
      metaFlagArray(addr.index)(0) := false.B.asTypeOf(new MetaFlagBundle) // dirty
    }

    // 读取当前字
    val origWord = dataReadData(0)(0)
    val newWordBytes = Wire(Vec(4, UInt(8.W)))
    val origBytes = origWord.asTypeOf(Vec(4, UInt(8.W)))
    val wdataBytes = req.wdata.asTypeOf(Vec(4, UInt(8.W)))
    for(i <- 0 until 4){
      newWordBytes(i) := Mux(req.wmask(i), wdataBytes(i), origBytes(i))
    }
    val newWord = newWordBytes.asUInt

    when(state === s_write_cache) {
      // 完整写入更新后的数据
      // dataArray(addr.index)(0) := VecInit(Seq(newWord))
      dataArray.io.wea := !isMMIO // 一致可缓存
      dataArray.io.addra := addr.index
      dataArray.io.dina := newWord
      
      // 同时更新metaArray
      // val writeMeta = Vec(Ways, new MetaBundle)
      // for (i <- 0 until Ways) {
      //   writeMeta(i).tag := addr.tag
      //   writeMeta(i).valid := true.B
      //   writeMeta(i).dirty := true.B
      // }
      // metaArray.write(addr.index, writeMeta)

      // metaArray(addr.index)(0).tag := addr.tag
      // metaArray(addr.index)(0).valid := true.B
      // metaArray(addr.index)(0).dirty := true.B
      metaArray.io.wea := !isMMIO // 一致可缓存
      metaArray.io.addra := addr.index
      metaArray.io.dina := addr.tag // tag
      metaValidArray.io.wea := !isMMIO // 一致可缓存
      metaValidArray.io.addra := addr.index
      metaValidArray.io.dina := true.B // valid
      metaFlagArray(addr.index)(0) := true.B.asTypeOf(new MetaFlagBundle) // dirty

      io.resp.valid := !flushed // 如果没有被flush过，则返回有效响应
    }

    // 将所需要的数据返回给load指令
    when(state === s_read_cache){
      resp.rdata := cacheData // 直接返回cache中的数据
      io.resp.valid := !flushed // 如果没有被flush过，则返回有效响应
    }

    when(isMMIO) {
      resp.rdata := io.axi.rdata
    }

    io.resp.bits := resp

    io.resp.bits.cmd := req.cmd // 保留命令类型

    // when(io.req.valid){
    //   printf("DCache: %x, %x, %x, %x\n", req.addr, req.wdata, addr.tag, addr.index)
    // }

    if(GenCtrl.USE_COUNT) {
      val counting = RegInit(false.B)
      when(io.req.valid) {
        counting := true.B
      }.elsewhen(io.resp.fire || io.flush) {
        counting := false.B
      }

      val delayCounter = RegInit(0.U(64.W))
      val lsCounter = RegInit(1.U(32.W))

      when(io.resp.fire) {
        lsCounter := lsCounter + 1.U
        printf("[LSU] Average LSU delay: %d cycles\n", delayCounter / lsCounter)
      }.elsewhen(counting) {
        delayCounter := delayCounter + 1.U
      }
    }
}