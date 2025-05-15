package core
import chisel3._
import chisel3.util._

object MmuConfig {
    def ADDR_WIDTH = 32
    def PAGE_WIDTH = 12 //4kb
    def TLB_NUM = 16
}