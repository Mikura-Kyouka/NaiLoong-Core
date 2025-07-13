package core

import chisel3._
import chisel3.util._

object IssueConfig {
  def PHYS_REG_NUM = 64
  def ISSUE_WIDTH = 5 // 2 ALU + 1 MDU + 1 LSU + 1 BRU
  def UNORDER_QUEUE_SIZE = 4
  def QUEUE_SIZE = 8
  def PHYS_REG_BITS = log2Ceil(PHYS_REG_NUM)
}