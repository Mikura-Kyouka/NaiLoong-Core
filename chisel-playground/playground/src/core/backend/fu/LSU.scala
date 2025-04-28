package core
import chisel3._
import chisel3.util._

import utils._

object LSUOpType {
  def lb   = "b0000000".U
  def lh   = "b0000001".U
  def lw   = "b0000010".U
  def lbu  = "b0000100".U
  def lhu  = "b0000101".U
  def sb   = "b0001000".U
  def sh   = "b0001001".U
  def sw   = "b0001010".U

  def ll   = "b0100000".U 
  def sc   = "b0100001".U
  def isAdd(func: UInt) = func(6)
  def isAtom(func: UInt): Bool = func(5)
  def isStore(func: UInt): Bool = func(3)
  def isLoad(func: UInt): Bool = !isStore(func) & !isAtom(func)
  def isLL(func: UInt): Bool = func === ll
  def isSC(func: UInt): Bool = func === sc
  // def isAMO(func: UInt): Bool = isAtom(func) && !isLR(func) && !isSC(func)

  // def needMemRead(func: UInt): Bool = isLoad(func) || isAMO(func) || isLR(func)
  // def needMemWrite(func: UInt): Bool = isStore(func) || isAMO(func) || isSC(func)

  def atomW = "010".U
  def atomD = "011".U
}

object MEMOpID {
  def idle   = "b0000_000".U
  def load   = "b0001_001".U
  def store  = "b0001_010".U
  def storec = "b0010_010".U //store commit
  def lr     = "b0001_101".U
  def sc     = "b0001_110".U
  def tlb    = "b0100_001".U

  def needLoad(memop: UInt) = memop(0)
  def needStore(memop: UInt) = memop(1)
  def commitToCDB(memop: UInt) = memop(3)
  def commitToSTQ(memop: UInt) = memop(4)
  def commitToTLB(memop: UInt) = memop(5)
}

trait HasLSUConst {
  val moqSize = 8
  val storeQueueSize = 8
}

class StoreQueueEntry extends Bundle {
  val pc = UInt(32.W)
  val prfidx = UInt(7.W)
  val valid = Bool()
}

class DCacheUserBundle extends Bundle {
  val moqidx = UInt(5.W)
  val op     = UInt(7.W)
}

class moqEntry extends Bundle{
  val pc = UInt(32.W)

  val func = UInt(7.W)
  val size = UInt(2.W)
  val op = UInt(7.W)
  val data = UInt(32.W)

  val fdata = UInt(32.W) // forwarding data
  val fmask = UInt(4.W) // forwarding mask

  val isMMIO = Bool()
  val rfWen = Bool()
  val valid = Bool()
  val finished = Bool()
  val storePageFault = Bool()
  val loadPageFault = Bool()
  val loadAddrMisaligned = Bool()
  val storeAddrMisaligned = Bool()
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(32.W))
  val axi = new AXI
  // tlb signals

  val uopIn = Input(new RenamedDecodeIO)
  val uopOut = Output(new RenamedDecodeIO)
  val isMMIO = Output(Bool())
}

class UnpipeLSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(32.W))
  val dmem = new AXI
  val robRetire = Input(Bool()) // Store insts
  val complete = Output(Bool()) // tell ROB to commit inst
  val isMMIO = Output(Bool())
  val dtlbPF = Output(Bool()) // TODO: refactor it for new backend
  val loadAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
  val storeAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
}

class UnpipelinedLSU extends Module with HasLSUConst {
  val io = IO(new UnpipeLSUIO)
  io := DontCare
  val dcache = Module(new DCache()(new DCacheConfig(totalSize = 4 * 16, ways = 1)))
  dcache.io.axi <> io.dmem
  dcache.io.req.valid := io.in.valid
  dcache.io.req.bits.addr := io.in.bits.src1 + io.in.bits.src2
  dcache.io.req.bits.wdata := io.wdata
  dcache.io.req.bits.wmask := MuxLookup(io.in.bits.func(1, 0), 0.U)(
    List(
    "b00".U -> "b0001".U,
    "b01".U -> "b0011".U,
    "b10".U -> "b1111".U
    )
  )
  dcache.io.req.bits.cmd := Mux(LSUOpType.isStore(io.in.bits.func), 1.U, 0.U)
  
  io.out.valid := dcache.io.resp.valid
  io.out.bits := dcache.io.resp.bits.rdata

  io.in.ready := dcache.io.req.ready && (!io.in.valid || io.out.fire)
}

class AligendUnpipelinedLSU extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(Output(new PipelineConnectIO)))
    val out = Decoupled(new FuOut)
    val lsAXI = new AXI
    val robCommit = Input(new RobCommit)
  })
  val lsu = Module(new UnpipelinedLSU)

  // load
  io.out.valid := lsu.io.complete

  // store
  val commitIdx = io.robCommit.commit.map(_.valid)
  val commitDatas = io.robCommit.commit.map(_.bits)
  lsu.io.robRetire := commitIdx.reduce(_ || _)
  lsu.io.wdata := PriorityMux(commitIdx, commitDatas).data

  lsu.io.dmem <> io.lsAXI
  lsu.io.dtlbPF := DontCare
  lsu.io.isMMIO := DontCare

  lsu.io.loadAddrMisaligned := DontCare
  lsu.io.storeAddrMisaligned := DontCare

  lsu.io.in.bits.src1 := io.in.bits.src1
  lsu.io.in.bits.src2 := Mux(io.in.bits.ctrl.src2Type === 1.U, io.in.bits.imm, io.in.bits.src2)
  lsu.io.in.bits.func := io.in.bits.ctrl.fuOpType

  io.out.bits := DontCare
  io.out.bits.pc := io.in.bits.pc
  io.out.bits.data := lsu.io.out.bits
  io.out.bits.robIdx := io.in.bits.robIdx
  
  lsu.io.in.valid := io.in.valid
  io.in.ready := lsu.io.in.ready
  io.out.valid := lsu.io.out.valid
  lsu.io.out.ready := io.out.ready
} 

