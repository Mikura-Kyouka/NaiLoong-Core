import chisel3._
import chisel3.util._
object IssueConfig {
  def PHYS_REG_NUM = 128
  def ISSUE_WIDTH = 4
  def QUEUE_SIZE = 16
  def PHYS_REG_BITS = log2Ceil(PHYS_REG_NUM)
}