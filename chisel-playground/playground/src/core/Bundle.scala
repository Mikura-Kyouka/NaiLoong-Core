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

class RenamedDecodeIO extends Bundle {
  val decode = new DecodeIO
  val prfDest = Output(UInt(7.W)) // TODO:prfAddrWidth
  val prfSrc1 = Output(UInt(7.W)) // TODO:prfAddrWidth
  val prfSrc2 = Output(UInt(7.W)) // TODO:prfAddrWidth
  val src1Rdy = Output(Bool())
  val src2Rdy = Output(Bool())
}

class AXI extends Bundle {
  /* AR */
  val arvalid = Output(Bool())
  val arready = Input(Bool())
  val araddr = Output(UInt(32.W))
  val arid = Output(UInt(4.W))  
  val arlen = Output(UInt(8.W))  
  val arsize = Output(UInt(3.W)) 
  val arburst = Output(UInt(2.W))  

  /* R */
  val rvalid = Input(Bool())
  val rready = Output(Bool())
  val rdata = Input(UInt(32.W))
  val rresp = Input(UInt(2.W))  
  val rlast = Input(Bool())  
  val rid = Input(UInt(4.W)) 

  /* AW */
  val awvalid = Output(Bool())
  val awready = Input(Bool())
  val awaddr = Output(UInt(32.W))
  val awid = Output(UInt(4.W)) // Write Address ID
  val awlen = Output(UInt(8.W)) // Burst length
  val awsize = Output(UInt(3.W)) // Burst size
  val awburst = Output(UInt(2.W)) // Burst type

  /* W */
  val wvalid = Output(Bool())
  val wready = Input(Bool())
  val wdata = Output(UInt(32.W))
  val wstrb = Output(UInt(4.W))
  val wlast = Output(Bool())  

  /* B */
  val bvalid = Input(Bool())
  val bready = Output(Bool())
  val bresp = Input(UInt(2.W))
  val bid = Input(UInt(4.W))  
}

class PipelineConnectIO extends Bundle {
  // common
  val instr = Output(UInt(32.W))
  val pc = Output(UInt(32.W))
  val valid = Output(Bool())
  // IF -> ID
  val pnpc = Output(UInt(32.W)) // TODO:VAddrBits
  val redirect = new RedirectIO
  val exceptionVec = Output(Vec(16, Bool()))
  val intrVec = Output(Vec(12, Bool()))
  val brIdx = Output(UInt(4.W))
  val crossPageIPFFix = Output(Bool())
  val isBranch = Output(Bool())
  // ID -> Rename
  val src1 = Output(UInt(32.W))
  val src2 = Output(UInt(32.W))
  val imm  = Output(UInt(32.W))
  val ctrl = new CtrlSignalIO
  // Rename -> Dispatch
  val prj      = Output(UInt(RegConfig.PHYS_REG_BITS.W))
  val jIsArf   = Output(Bool())
  val dataj    = Output(UInt(32.W))
  val prk      = Output(UInt(RegConfig.PHYS_REG_BITS.W))
  val kIsArf   = Output(Bool())
  val datak    = Output(UInt(32.W))
  val preg     = Output(UInt(RegConfig.PHYS_REG_BITS.W))
  val old_preg = Output(UInt(RegConfig.PHYS_REG_BITS.W))

  // How to classify the checkpoint???
  val checkpoint = new Bundle {
    val needSave = Bool()
    val id = UInt(64.W)
  }
  
  // ROB
  val robIdx = Output(UInt(RobConfig.ROB_INDEX_WIDTH.W))

}
