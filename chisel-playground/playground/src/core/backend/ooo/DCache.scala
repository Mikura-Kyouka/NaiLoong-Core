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
    val failsc = Input(Bool())
    val cacopOp = Input(UInt(2.W))
    val cacopEn = Input(Bool())
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
  val LineSize = 16 // byte
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
        // val cacop = Input(new CACOPIO)
    })
    val reqReg = RegEnable(io.req.bits, io.req.fire)
    val req = Mux(io.req.fire, io.req.bits, reqReg)

    val cacopOp0 = req.cacopEn && req.cacopOp === CACOPOp.op0
    val cacopOp1 = req.cacopEn && req.cacopOp === CACOPOp.op1
    val cacopOp2 = req.cacopEn && req.cacopOp === CACOPOp.op2
    dontTouch(cacopOp0)
    dontTouch(cacopOp1)
    dontTouch(cacopOp2)

    val resp = Wire(new respBundle)
    resp := DontCare
    resp.moqIdx := req.moqIdx // 保留moq entry index
    val addr = req.addr.asTypeOf(addrBundle)
    val failsc = req.failsc 
    // when(cacopOp0 || cacopOp1) {
    //   addr := io.cacop.VA.asTypeOf(addrBundle)
    // }

    val isMMIO = req.isMMIO //FIXME：
    // val isMMIO = false.B

    //    0          1              2               3             4                  5               6               7               8
    val s_idle :: s_judge :: s_write_cache :: s_read_cache :: s_write_mem1 :: s_write_mem2 :: s_write_mem3 :: s_read_mem1 :: s_read_mem2 :: Nil = Enum(9)

    val state = RegInit(s_idle)

    // 暂时只支持 1 way
    val metaArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * (TagBits)))
    val metaValidArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * 1))
    // val metaFlagArray = RegInit(VecInit(Seq.fill(Sets)(VecInit(Seq.fill(Ways)(0.U.asTypeOf(new MetaFlagBundle))))))
    val metaFlagArray = Module(new DualPortBRAM(log2Ceil(Sets), Ways * 1))
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

    metaFlagArray.io.clka := clock
    metaFlagArray.io.addra := addr.index
    metaFlagArray.io.dina := 0.U // 后续覆盖
    metaFlagArray.io.wea := false.B // 后续覆盖
    metaFlagArray.io.addrb := addr.index

    val metaReadData = metaArray.io.doutb.asTypeOf(Vec(Ways, new MetaBundle))
    // val syncReadAddr = RegInit(0.U(log2Ceil(Sets).W))
    // val is_collision = RegInit(false.B)
    // val collison_data = RegInit(0.U.asTypeOf(VecInit(Seq.fill(Ways)(0.U.asTypeOf(new MetaFlagBundle)))))
    // syncReadAddr := addr.index
    // is_collision := (io.axi.rvalid && io.axi.rlast) && state === s_read_mem2 && !isMMIO || state === s_write_cache
    // collison_data := Mux((io.axi.rvalid && io.axi.rlast) && state === s_read_mem2 && !isMMIO, VecInit(Seq.fill(Ways)(false.B.asTypeOf(new MetaFlagBundle))), 
    //                                                                         VecInit(Seq.fill(Ways)(true.B.asTypeOf(new MetaFlagBundle))))

    // val metaFlagData = Mux(is_collision, collison_data, metaFlagArray(syncReadAddr))
    val metaFlagData = metaFlagArray.io.doutb.asTypeOf(Vec(Ways, Bool()))
    val metaValidData = metaValidArray.io.doutb.asTypeOf(Vec(Ways, Bool()))
    val dataReadData = dataArray.io.doutb.asTypeOf(Vec(Ways, Vec(LineBeats, UInt(32.W))))

    val hitVec = VecInit(
      metaReadData.map(m => m.tag === addr.tag && metaValidData(0))
    ).asUInt

    val hit = hitVec.orR

    val dirty = metaFlagData(0)// .dirty

    val flushed = RegInit(false.B) // 用于标记当前事务是否已经被flush过
    when(io.req.fire) {
      flushed := false.B
    }
    when(io.flush) {
      flushed := true.B
    }

    val wburst = RegInit(0.U(WordIndexBits.W))

    // reference: 《SuperScalar RISC Processor Design》 P. 103
    // hit -> write/read dataArray
    // !hit -> find one line -> dirty? --yes--> write this dirty cacheline to mem -> read from mem to cache -> read/write cache
    //                            +-------------------no----------------------------------+
    // NOTE: Write need to load first and then write, 
    // because store may only write to specific byte

    state := MuxLookup(state, s_idle)(Seq(
        s_idle -> Mux(io.req.fire && !cacopOp0 && !failsc, 
                        Mux(isMMIO, Mux(req.cmd, s_write_mem1, s_read_mem1), s_judge), 
                        s_idle),
        s_judge -> Mux(hit, Mux(cacopOp1 || cacopOp2, Mux(dirty, s_write_mem1, s_idle), Mux(req.cmd, s_write_cache, s_read_cache)), 
                            Mux(dirty, s_write_mem1, Mux(cacopOp1 || cacopOp2, s_idle, s_read_mem1))), 
        s_write_mem1 -> Mux(io.axi.awready, s_write_mem2, s_write_mem1),
        s_write_mem2 -> Mux((io.axi.wready && (wburst === 3.U || isMMIO)), s_write_mem3, s_write_mem2),
        s_write_mem3 -> Mux(io.axi.bvalid, Mux(isMMIO || cacopOp1 || cacopOp2, s_idle, s_read_mem1), s_write_mem3),
        s_read_mem1 -> Mux(io.axi.arready, s_read_mem2, s_read_mem1),
        s_read_mem2 -> Mux((io.axi.rvalid && io.axi.rlast), Mux(isMMIO, s_idle, Mux(req.cmd, s_write_cache, s_read_cache)), s_read_mem2), // FIXME:
        s_write_cache -> s_idle,
        s_read_cache -> s_idle
    ))

    io.req.ready := state === s_idle
    io.resp.valid := (((isMMIO && io.axi.rvalid) || 
                      (isMMIO && io.axi.bvalid) || 
                      ((cacopOp1 || cacopOp2) && state === s_write_mem3) ||
                      (hit && cacopOp2 && !dirty && state === s_judge) ||
                      (!hit && cacopOp2 && state === s_judge)) && !flushed) || 
                     (io.req.valid && cacopOp0 && state === s_idle) || 
                     (io.req.valid && cacopOp1 && !dirty && state === s_judge) || 
                     (io.req.valid && failsc && state === s_idle)

    io.resp.bits.resp := false.B
    io.resp.bits.rdata := 0.U(32.W)
    io.axi := DontCare

    when(cacopOp0) {
      metaArray.io.wea := true.B
      metaArray.io.addra := addr.index
      metaArray.io.dina := 0.U

      // metaFlagArray(addr.index)(0) := false.B.asTypeOf(new MetaFlagBundle) // dirty
      metaFlagArray.io.wea := true.B
      metaFlagArray.io.addra := addr.index
      metaFlagArray.io.dina := false.B
    }

    // when(cacopOp1 || (cacopOp2 && hit)) {
    //   metaArray.io.wea := true.B
    //   metaArray.io.addra := addr.index
    //   metaArray.io.dina := 0.U
    //   metaFlagArray(addr.index)(0) := false.B.asTypeOf(new MetaFlagBundle) // dirty
    // }

    // val offset = req.addr(1, 0) << 3
    // val wburst = RegInit(0.U(WordIndexBits.W))
    val cacheData = dataReadData(0)(wburst).asUInt
    // axi read chanel
    io.axi.arvalid := state === s_read_mem1
    io.axi.araddr := Mux(isMMIO, req.addr, Cat(addr.asUInt(31, OffsetBits), 0.U(OffsetBits.W)))// 32 bits address
    io.axi.arid := 1.U(4.W)
    io.axi.arlen := Mux(isMMIO, 0.U, (LineBeats - 1).asUInt) // Burst_Length = AxLEN[7:0] + 1
    io.axi.arsize := Mux(isMMIO, Cat(0.U(1.W), req.size), "b010".U)
    io.axi.arburst := "b01".U
    io.axi.rready := true.B
    // axi write chanel
    val awaddr = Cat(metaReadData(0).tag, addr.index, 0.U(OffsetBits.W))
    io.axi.awaddr := Mux(isMMIO, req.addr, awaddr)
    io.axi.awvalid := state === s_write_mem1
    io.axi.awid := 1.U(4.W)
    io.axi.awlen := Mux(isMMIO, 0.U, (LineBeats - 1).asUInt)
    io.axi.awsize := Mux(isMMIO, Cat(0.U(1.W), req.size), "b010".U) // 32 bits
    io.axi.awburst := "b01".U
    io.axi.wvalid := state === s_write_mem2
    io.axi.wlast := isMMIO
    io.axi.wid := 1.U(4.W)
    io.axi.wstrb := Mux(isMMIO, req.wmask, "b1111".U) 
    io.axi.wdata := Mux(isMMIO, req.wdata, cacheData)
    io.axi.bready := state === s_write_mem3
    // 从下级存储器读取Data Block到Cache刚刚被选定的line中 
    // 将这个cacheline标记为not dirty的状态
    val burst = RegInit(0.U(WordIndexBits.W))
    val dataLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
    val rdata = io.axi.rdata

    val axiDataLatch = RegInit(VecInit(Seq.fill(LineBeats)(0.U(32.W))))
    when(io.axi.rvalid && state === s_read_mem2 && !isMMIO) {
      burst := burst + 1.U
      axiDataLatch(burst) := rdata
      dataLatch(burst) := rdata
    }

    when((io.axi.rlast && io.axi.rvalid) && state === s_read_mem2 && !isMMIO) {
      burst := 0.U 
      // data
      dataArray.io.wea := !isMMIO
      dataArray.io.addra := addr.index
      dataArray.io.dina := Cat(rdata, axiDataLatch(2), axiDataLatch(1), axiDataLatch(0))
      // tag
      metaArray.io.wea := !isMMIO
      metaArray.io.addra := addr.index
      metaArray.io.dina := addr.tag 
      // valid
      metaValidArray.io.wea := !isMMIO 
      metaValidArray.io.addra := addr.index
      metaValidArray.io.dina := true.B
      // dirty
      // metaFlagArray(addr.index)(0) := false.B.asTypeOf(new MetaFlagBundle) // dirty
      metaFlagArray.io.wea := true.B
      metaFlagArray.io.addra := addr.index
      metaFlagArray.io.dina := false.B
    }

    // io.axi.wlast := false.B
    when(io.axi.wready && io.axi.wvalid && state === s_write_mem2 && !isMMIO) {
      wburst := wburst + 1.U
    }

    when(wburst === 3.U) {
      io.axi.wlast := true.B
    }

    when(io.axi.bvalid) {
      wburst := 0.U
    }

    // 读取当前字
    val originalLine = dataReadData(0)
    val origWord = originalLine(addr.WordIndex)
    val newWordBytes = Wire(Vec(4, UInt(8.W)))
    val origBytes = origWord.asTypeOf(Vec(4, UInt(8.W)))
    val wdataBytes = req.wdata.asTypeOf(Vec(4, UInt(8.W)))
    for(i <- 0 until 4){
      newWordBytes(i) := Mux(req.wmask(i), wdataBytes(i), origBytes(i))
    }
    val newWord = newWordBytes.asUInt

    val updatedLine = Wire(Vec(originalLine.length, UInt(32.W)))
    updatedLine := originalLine
    updatedLine(addr.WordIndex) := newWord

    when(state === s_write_cache) {
      // data
      dataArray.io.wea := !isMMIO // 一致可缓存
      dataArray.io.addra := addr.index
      dataArray.io.dina := updatedLine.asUInt // 更新后的数据
      // tag
      metaArray.io.wea := !isMMIO // 一致可缓存
      metaArray.io.addra := addr.index
      metaArray.io.dina := addr.tag // tag
      // valid
      metaValidArray.io.wea := !isMMIO // 一致可缓存
      metaValidArray.io.addra := addr.index
      metaValidArray.io.dina := true.B // valid
      // dirty
      // metaFlagArray(addr.index)(0) := true.B.asTypeOf(new MetaFlagBundle) // dirty
      metaFlagArray.io.wea := true.B
      metaFlagArray.io.addra := addr.index
      metaFlagArray.io.dina := true.B

      io.resp.valid := !flushed // 如果没有被flush过，则返回有效响应
    }

    // 将所需要的数据返回给load指令
    when(state === s_read_cache){
      resp.rdata := dataReadData(0)(addr.WordIndex) // 直接返回cache中的数据
      io.resp.valid := !flushed // 如果没有被flush过，则返回有效响应
    }

    when(isMMIO) {
      resp.rdata := io.axi.rdata
    }

    when((cacopOp1 || cacopOp2) && hit && state === s_judge) {
      // valid
      metaValidArray.io.wea := true.B
      metaValidArray.io.addra := addr.index
      metaValidArray.io.dina := 0.U
      // dirty
      // metaFlagArray(addr.index)(0) := false.B.asTypeOf(new MetaFlagBundle) // dirty
      metaFlagArray.io.wea := true.B
      metaFlagArray.io.addra := addr.index
      metaFlagArray.io.dina := false.B
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
      val foo = RegInit(0.U(32.W))

      when(io.resp.fire) {
        lsCounter := lsCounter + 1.U
        foo := (foo + 1.U) % 100.U
        when(foo === 0.U) {
          printf("[DCache] Average DCache delay: %d cycles\n", delayCounter / lsCounter)
        }
      }.elsewhen(counting) {
        delayCounter := delayCounter + 1.U
      }
    }

    if(GenCtrl.USE_COUNT) {
      // 1. 周期计数器
      val cycle_counter = RegInit(0.U(32.W))
      cycle_counter := cycle_counter + 1.U

      // 2. 写回计数器
      val writeback_counter = RegInit(0.U(32.W))
      when(io.axi.bvalid && state === s_write_mem3 && !isMMIO) {
        writeback_counter := writeback_counter + 1.U
      }

      // 3. 每隔500周期打印统计信息
      when(cycle_counter % 500.U === 0.U && cycle_counter =/= 0.U) {
        printf(p"  Writeback: ${writeback_counter} times\n")
      }
    }
}