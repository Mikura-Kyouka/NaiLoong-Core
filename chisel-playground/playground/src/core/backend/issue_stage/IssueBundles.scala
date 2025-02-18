import chisel3._
import chisel3.util._
import IssueConfig._

class inst_info extends Bundle {
  val preg0 = UInt(PHYS_REG_BITS.W)
  val preg1 = UInt(PHYS_REG_BITS.W)
  val dest = UInt(PHYS_REG_BITS.W)
  val op = UInt(3.W)
}

class renaming_to_issue extends Bundle {
  val inst = new inst_info
}

class issue_to_execute extends Bundle {
  val inst = new inst_info
}

class dispatch_in_info extends Bundle {
  val inst = new inst_info
}

class dispatch_out_info extends Bundle {
  val inst_vec = Vec(ISSUE_WIDTH, new inst_info)
  val inst_cnt = UInt((log2Ceil(ISSUE_WIDTH) + 1).W)
}

class can_issue_bundle extends Bundle {
  val can_issue = Input(Vec(QUEUE_SIZE, Bool()))
  val preg0_vec = Output(Vec(QUEUE_SIZE, UInt(PHYS_REG_BITS.W)))
  val preg1_vec = Output(Vec(QUEUE_SIZE, UInt(PHYS_REG_BITS.W)))
}

class retire_inst_info extends Bundle {
  val inst = new inst_info
  val valid = Bool()
}