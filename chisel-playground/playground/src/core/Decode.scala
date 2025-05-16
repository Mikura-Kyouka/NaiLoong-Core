package core

import chisel3._
import chisel3.util._

object ImmType {
  def si12    = "b000".U  
  def ui12    = "b001".U  
  def si14_pc = "b010".U  
  def si16_pc = "b011".U  
  def si20    = "b100".U  
  def si26_pc = "b101".U  
  def nop     = "b111".U
  def apply = UInt(3.W)
}

object Dest {
  def r1 = "b00".U
  def rj = "b01".U
  def rd = "b10".U
  def apply = UInt(2.W)
}

object SrcType {
  def reg = "b0".U
  def pc  = "b1".U
  def imm = "b1".U
  def apply() = UInt(1.W)
}

object RfWen {
  def y = "b0".U
  def n = "b1".U
  def apply = UInt(1.W)
}

object SrcIsRd {
  val y = "b0".U
  val n = "b1".U
  def apply = UInt(1.W)
}

object IsLegal {
  def y = "b0".U
  def n = "b1".U
  def apply = UInt(1.W)
}

object FuType {
  def num = 5 
  def alu = "b000".U
  def lsu = "b001".U
  def mdu = "b010".U
  def csr = "b011".U
  def bru = "b101".U
  def apply() = UInt(log2Up(num).W)
}

object CSROp {
  def nop = "b000".U
  def rd = "b001".U
  def wr = "b010".U
  def xchg = "b011".U
}

object FuOpType {
  def apply() = UInt(7.W)
}

object Instructions {
  def NOP = 0x03400000.U   // andi r0, r0, 0
  val DecodeDefault = List(ImmType.nop, FuType.alu, ALUOpType.add, SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.n, CSROp.nop)  
  def DecodeTable = Inst.table
}