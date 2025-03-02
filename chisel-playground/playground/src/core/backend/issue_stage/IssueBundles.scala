import chisel3._
import chisel3.util._
import IssueConfig._

class inst_info extends Bundle {
  val areg1 = UInt(5.W)
  val areg2 = UInt(5.W)
  val preg0 = UInt(PHYS_REG_BITS.W)
  val preg1 = UInt(PHYS_REG_BITS.W)
  val dest = UInt(PHYS_REG_BITS.W)
  val op = UInt(3.W)

  // use imm
  val imm = UInt(32.W)
  val src2_is_imm = Bool()

  // is retire
  val src1_is_areg = Bool()
  val src2_is_areg = Bool()
}

// 相当于给inst_info改名
class renaming_to_issue extends inst_info {
}

class issue_to_execute extends inst_info {
}

class dispatch_in_info extends inst_info {
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

class commit_inst_info extends Bundle {
  val inst = new inst_info
  val valid = Bool()
}

class retire_inst_info extends Bundle {
  val preg = UInt(PHYS_REG_BITS.W)
  val areg = UInt(5.W)
  val valid = Bool()
}