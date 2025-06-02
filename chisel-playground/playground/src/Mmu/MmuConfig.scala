package core
import chisel3._
import chisel3.util._

object MemType {
  val fetch   = 3.U(2.W)
  val load    = 1.U(2.W)
  val store   = 2.U(2.W)
  def apply() = UInt(2.W)
}

object MmuConfig {
    def ADDR_WIDTH = 32
    def PAGE_WIDTH = 12 //4kb
    def TLB_NUM = 16
}