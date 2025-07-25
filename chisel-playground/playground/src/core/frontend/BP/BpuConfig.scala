package core
import chisel3._
import chisel3.util._

object BpuConfig {
  val ISSUE_WIDTH = 4
  val INDEX_WIDTH = 10
  val HISTORY_WIDTH = 6
  val BTB_INDEX_WIDTH = 10
  val BTB_DATA_WIDTH = 34 // 32 目标 + 2 flag
  val RAS_DEPTH  = 16
  val RAS_WIDTH  = log2Ceil(RAS_DEPTH)
}

