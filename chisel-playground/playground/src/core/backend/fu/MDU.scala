package core

import chisel3._
import chisel3.util._

object MDUOpType {
  def mul    = "b000".U
  def mulh   = "b001".U
  def mulhu  = "b011".U
  def div    = "b100".U
  def divu   = "b101".U
  def mod    = "b110".U
  def modu   = "b111".U

  def isDiv(op: UInt) = op(2)
  def isDivSign(op: UInt) = isDiv(op) && !op(0)
}

class MulDivIO(val len: Int) extends Bundle {
  val in = Flipped(DecoupledIO(Vec(2, Output(UInt(len.W)))))
  val sign = Input(Bool())
  val out = DecoupledIO(Output(UInt((len * 2).W)))
}

class Multiplier(len: Int) extends Module {
  val io = IO(new MulDivIO(len))

}

class Divider(len: Int = 64) extends Module {
  val io = IO(new MulDivIO(len))

}

class MDUIO extends FunctionUnitIO {

}

class MDU extends Module {
  val io = IO(new MDUIO)

  val mul = Module(new Multiplier(32 + 1)) // XLEN + 1
  val div = Module(new Divider(32)) // XLEN
  
}