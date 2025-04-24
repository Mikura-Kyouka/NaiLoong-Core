package core

import core._ 
import chisel3._
import chisel3.util._ 

import utils._
import core.ALUOpType.add

object ALUOpType {
  def add  = "b1000000".U
  def sll  = "b0000001".U
  def slt  = "b0000010".U
  def sltu = "b0000011".U
  def xor  = "b0000100".U
  def srl  = "b0000101".U
  def or   = "b0000110".U
  def nor  = "b0000000".U
  def and  = "b0000111".U
  def sub  = "b0001000".U
  def sra  = "b0001101".U

  def jirl = "b1011000".U // link 
  def b    = "b1011001".U // no-link 
  def bl   = "b1011010".U // link
  def beq  = "b0010000".U 
  def bne  = "b0010001".U
  def blt  = "b0010100".U
  def bge  = "b0010101".U
  def bltu = "b0010110".U
  def bgeu = "b0010111".U

  // for RAS
  def call = "b1011100".U
  def ret  = "b1011110".U

  def isAdd(func: UInt) = func(6)
  def isBru(func: UInt) = func(4)
  def isBranch(func: UInt) = !func(3)
  def isJump(func: UInt) = isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 1)
  def isBranchInvert(func: UInt) = func(0)
}

// class FunctionUnitIO extends Bundle {
//   val in = Flipped(Decoupled(new Bundle {
//     val src1 = Output(UInt(32.W))
//     val src2 = Output(UInt(32.W))
//     val func = Output(FuOpType())
//   }))
//   val out = Decoupled(Output(UInt(32.W)))
// } 
// class CtrlFlowIO extends Bundle {
//   val instr = Output(UInt(32.W))
//   val pc = Output(UInt(32.W)) // TODO:VAddrBits
//   val pnpc = Output(UInt(32.W)) // TODO:VAddrBits
//   val redirect = new RedirectIO
//   val exceptionVec = Output(Vec(16, Bool()))
//   val intrVec = Output(Vec(12, Bool()))
//   val brIdx = Output(UInt(4.W))
//   val crossPageIPFFix = Output(Bool())
//   val runahead_checkpoint_id = Output(UInt(64.W))
//   val isBranch = Output(Bool())
// }
class ALUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val pc = Input(UInt(32.W))
  val redirect = new RedirectIO
  val offset = Input(UInt(32.W))
}

class ALU extends Module {
  val io = IO(new ALUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isAdderSub = !ALUOpType.isAdd(func) // sub 
  val adderRes = (src1 +& (src2 ^ Fill(32, isAdderSub))) + isAdderSub
  dontTouch(adderRes)
  val xorRes = src1 ^ src2
  val sltu = !adderRes(32)
  val slt = xorRes(31) ^ sltu

  val shsrc1 = src1
  val shamt = src2(4, 0) //shift amount
  val res = MuxLookup(func, adderRes)(
    List(
      ALUOpType.sll  -> ((shsrc1  << shamt)(31, 0)),
      ALUOpType.slt  -> ZeroExt(slt, 32),
      ALUOpType.sltu -> ZeroExt(sltu, 32),
      ALUOpType.xor  -> xorRes,
      ALUOpType.srl  -> (shsrc1  >> shamt),
      ALUOpType.or   -> (src1  |  src2),
      ALUOpType.nor  -> ~(src1  |  src2), 
      ALUOpType.and  -> (src1  &  src2),
      ALUOpType.sra  -> ((shsrc1.asSInt >> shamt).asUInt)
    )
  )
  val aluRes = res

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR, // .orR:所有位都是0,返回false
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt, 
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  ) // note: here we only have 3 ALUOp as we use invert to expand them to 6(all)

  val isBranch = ALUOpType.isBranch(func)
  val isBru = ALUOpType.isBru(func) //Branch Resolution

  // object LookupTree {
  // def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
  //   Mux1H(mapping.map(p => (p._1 === key, p._2)))
  // }
  
  val branchType = ALUOpType.getBranchType(func)
  dontTouch(branchType)
  val taken = LookupTree(ALUOpType.getBranchType(func), branchOpTable) || 
              (ALUOpType.isBranchInvert(func) && !LookupTree(ALUOpType.getBranchType(func), branchOpTable)) // branch taken

  // if branch type, condition calculation takes over alu, we use another adder
  // else(b, bl, jirl) we use adderRes which calculate dnpc.
  val what = io.pc + io.offset
  dontTouch(what)
  dontTouch(adderRes)
  val target = Mux(isBranch, io.pc + io.offset, adderRes)
  // val predictWrong = Mux(!taken && isBranch, io.cfIn.brIdx(0), !io.cfIn.brIdx(0) || (io.redirect.target =/= io.cfIn.pnpc)) //是分支指令但是不跳转
  // TODO: Temperarily no branch prediction , we assume predictWrong is always true
  val predictWrong = true.B
  dontTouch(target)
  dontTouch(taken)
  dontTouch(isBranch)
  io.redirect.target := Mux(!taken && isBranch, io.pc + 4.U, target) // branch not taken, pc changes to snpc, else to target
  io.redirect.valid := valid && isBru && predictWrong
  // val redirectRtype = if (EnableOutOfOrderExec) 1.U else 0.U
  // io.redirect.rtype := redirectRtype
  io.redirect.rtype := DontCare // TODO
  // actually for bl and jirl to write pc + 4 to rd 
  dontTouch(aluRes)
  io.out.bits := Mux(isBru, io.pc + 4.U, aluRes) // out only has a single 32-bit field
  
  io.in.ready := io.out.ready
  io.out.valid := valid 
}
/*
class inst_info extends Bundle {
  val areg1 = UInt(5.W)
  val areg2 = UInt(5.W)
  val preg1 = UInt(PHYS_REG_BITS.W)
  val preg2 = UInt(PHYS_REG_BITS.W)
  val data1 = UInt(32.W)
  val data2 = UInt(32.W)
  val dest = UInt(PHYS_REG_BITS.W)
  val op = UInt(3.W)

  // use imm
  val imm = UInt(32.W)
  val src2_is_imm = Bool()
}
*/

class FuOut extends Bundle {
  val pc     = Output(UInt(32.W))
  val data   = Output(UInt(32.W))
  val robIdx = Output(UInt(RobConfig.ROB_INDEX_WIDTH.W))
  val redirect = Output(new RedirectIO)
}
class AligendALU extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(Output(new PipelineConnectIO)))
    val out = Decoupled(new FuOut)
  })
  
  dontTouch(io.in.bits)
  val alu = Module(new ALU)
  alu.io := DontCare
  alu.io.in.bits.src1 := io.in.bits.dataj
  alu.io.in.bits.src2 := Mux(io.in.bits.ctrl.src2Type === 1.U, io.in.bits.imm, io.in.bits.datak)
  alu.io.in.bits.func := io.in.bits.ctrl.fuOpType
  alu.io.offset       := io.in.bits.imm
  alu.io.pc           := io.in.bits.pc
  io.out.bits.pc      := io.in.bits.pc
  io.out.bits.data    := alu.io.out.bits
  io.out.bits.robIdx  := io.in.bits.robIdx
  io.out.bits.redirect := alu.io.redirect
  
  alu.io.in.valid := io.in.valid
  io.in.ready := alu.io.in.ready
  io.out.valid := alu.io.out.valid
  alu.io.out.ready := io.out.ready
} 