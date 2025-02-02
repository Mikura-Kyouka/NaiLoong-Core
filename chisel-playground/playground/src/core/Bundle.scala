package core 

import chisel3._ 
import chisel3.util._ 

class CtrlSignalIO extends Bundle {
  val src1Type = Output(SrcType())
  val src2Type = Output(SrcType())
  val fuType = Output(FuType())
  val fuOpType = Output(FuOpType())
  val rfSrc1 = Output(UInt(5.W))
  val rfSrc2 = Output(UInt(5.W))
  val rfWen = Output(Bool())
  val rfDest = Output(UInt(5.W))
  val isNutCoreTrap = Output(Bool())
  val isSrc1Forward = Output(Bool())
  val isSrc2Forward = Output(Bool())
  val noSpecExec = Output(Bool())  // This inst can not be speculated
  val isBlocked = Output(Bool())   // This inst requires pipeline to be blocked
}

class DataSrcIO extends Bundle {
  val src1 = Output(UInt(32.W))
  val src2 = Output(UInt(32.W))
  val imm  = Output(UInt(32.W))
}

class RedirectIO extends Bundle {
  val target = Output(UInt(32.W)) // TODO:VAddrBits
  val rtype = Output(UInt(1.W)) // 1: branch mispredict: only need to flush frontend  0: others: flush the whole pipeline
  val valid = Output(Bool())
}

class CtrlFlowIO extends Bundle {
  val instr = Output(UInt(32.W))
  val pc = Output(UInt(32.W)) // TODO:VAddrBits
  val pnpc = Output(UInt(32.W)) // TODO:VAddrBits
  val redirect = new RedirectIO
  val exceptionVec = Output(Vec(16, Bool()))
  val intrVec = Output(Vec(12, Bool()))
  val brIdx = Output(UInt(4.W))
  val crossPageIPFFix = Output(Bool())
  val runahead_checkpoint_id = Output(UInt(64.W))
  val isBranch = Output(Bool())
}

class DecodeIO extends Bundle {
  val cf = new CtrlFlowIO
  val ctrl = new CtrlSignalIO
  val data = new DataSrcIO
}

class FunctionUnitIO extends Bundle {
  val in = Flipped(Decoupled(new Bundle {
    val src1 = Output(UInt(32.W))
    val src2 = Output(UInt(32.W))
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(32.W)))
} 