package core

import chisel3._
import chisel3.util._

import IssueConfig._

class inst_info extends Bundle {
  val areg1 = UInt(5.W)
  val areg2 = UInt(5.W)
  val preg1 = UInt(PHYS_REG_BITS.W)
  val preg2 = UInt(PHYS_REG_BITS.W)
  val data1 = UInt(32.W)
  val data2 = UInt(32.W)
  val dest = UInt(PHYS_REG_BITS.W)
  val op = FuOpType()

  // use imm
  val imm = UInt(32.W)
  val src2_is_imm = Bool()
}

// 相当于给inst_info改名
class renaming_to_issue extends inst_info {
}

class issue_to_execute extends inst_info {
}

class dispatch_in_info extends inst_info {
}

class dispatch_out_info extends Bundle {
  val inst_vec = Vec(ISSUE_WIDTH, Output(new inst_info))
  val inst_cnt = Output(UInt((log2Ceil(ISSUE_WIDTH) + 1).W))
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
  val data = UInt(32.W)
  val valid = Bool()
}

class payloadram_read_info extends Bundle {
  val src1 = Input(UInt(PHYS_REG_BITS.W))
  val src2 = Input(UInt(PHYS_REG_BITS.W))
  val pram_data1 = Output(UInt(32.W))
  val pram_data2 = Output(UInt(32.W))
}

class payloadram_write_info extends Bundle {
  val dest = Input(UInt(PHYS_REG_BITS.W))
  val pram_data = Input(UInt(32.W))
  val valid = Input(Bool())
}