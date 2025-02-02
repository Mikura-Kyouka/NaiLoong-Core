package core

import chisel3._
import chisel3.util._

object CSROpType {
  def jmp  = "b000".U
  def wrt  = "b001".U
  def set  = "b010".U
  def clr  = "b011".U
  def wrti = "b101".U
  def seti = "b110".U
  def clri = "b111".U
}

class CSRIO extends FunctionUnitIO {

}

class CSR extends Module {
    val io = IO (new CSRIO)
    val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
}