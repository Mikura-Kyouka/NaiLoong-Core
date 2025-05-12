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

  // val mul = Module(new Multiplier(32 + 1)) // XLEN + 1
  // val div = Module(new Divider(32)) // XLEN
  io.out.bits := 0.U 
  io.in.ready := !io.in.valid || io.out.fire
  io.out.valid := io.in.valid
}

class AlignedMDU extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(Output(new PipelineConnectIO)))
    val out = Decoupled(new FuOut)
  })
  val mdu = Module(new MDU)
  mdu.io := DontCare
  mdu.io.in.bits.src1 := io.in.bits.src1
  mdu.io.in.bits.src2 := Mux(io.in.bits.ctrl.src2Type === 1.U, io.in.bits.imm, io.in.bits.src2)
  mdu.io.in.bits.func := io.in.bits.ctrl.fuOpType
  io.out.bits.pc := io.in.bits.pc
  io.out.bits.data := mdu.io.out.bits
  io.out.bits.robIdx := io.in.bits.robIdx
  
  mdu.io.in.valid := io.in.valid
  mdu.io.out.ready := io.out.ready
  io.out.bits.redirect := DontCare
  io.in.ready := mdu.io.in.ready
  io.out.valid := mdu.io.out.valid

  io.out.bits.data := MuxLookup(io.in.bits.ctrl.fuOpType, 0.U)(Seq(
    MDUOpType.div -> ((io.in.bits.src1.asSInt / io.in.bits.src2.asSInt).asUInt),
    MDUOpType.divu -> io.in.bits.src1 / io.in.bits.src2,
    MDUOpType.mul -> ((io.in.bits.src1.asSInt * io.in.bits.src2.asSInt).asUInt),
    MDUOpType.mulh -> ((io.in.bits.src1.asSInt * io.in.bits.src2.asSInt) >> 32).asUInt,
    MDUOpType.mulhu -> ((io.in.bits.src1 * io.in.bits.src2) >> 32).asUInt,
    MDUOpType.mod -> ((io.in.bits.src1.asSInt % io.in.bits.src2.asSInt).asUInt),
    MDUOpType.modu -> (io.in.bits.src1 % io.in.bits.src2)
  ))
} 