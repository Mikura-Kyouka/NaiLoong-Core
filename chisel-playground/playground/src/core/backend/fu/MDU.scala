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
  
  val in_ready = RegInit(true.B)
  val out_valid = RegInit(true.B)

  val idle :: put :: waiting :: Nil = Enum(3)
  val div = RegInit(0.U(32.W))
  val divu = RegInit(0.U(32.W))
  val mod = RegInit(0.U(32.W))
  val modu = RegInit(0.U(32.W))

  val sdiv = new SignedDivider()
  sdiv.io.aclk := clock
  sdiv.io.s_axis_dividend_tdata := io.in.bits.src1.asSInt
  sdiv.io.s_axis_divisor_tdata  := io.in.bits.src2.asSInt

  val udiv = new UnsignedDivider()
  udiv.io.aclk := clock
  udiv.io.s_axis_dividend_tdata := io.in.bits.src1
  udiv.io.s_axis_divisor_tdata  := io.in.bits.src2

  val state_s = RegInit(idle)

  switch(state_s) {
    is(idle) {
      when(io.in.valid && MDUOpType.isDiv(io.in.bits.ctrl.fuOpType) && MDUOpType.isDivSign(io.in.bits.ctrl.fuOpType)) {
        out_valid := false.B
        sdiv.io.s_axis_dividend_tvalid := true.B
        sdiv.io.s_axis_divisor_tvalid := true.B
        in_ready := false.B
        state_s := put
      }
    }
    is(put) {
      when(sdiv.io.s_axis_dividend_tready && sdiv.io.s_axis_divisor_tready) {
        sdiv.io.s_axis_dividend_tvalid := false.B
        sdiv.io.s_axis_divisor_tvalid := false.B
        sdiv.io.m_axis_dout_tready := true.B
        state_s := waiting
      }
    }
    is(waiting) {
      when(sdiv.io.m_axis_dout_tvalid) {
        div := sdiv.io.m_axis_dout_tdata(63, 32)
        mod := sdiv.io.m_axis_dout_tdata(31, 0)
        sdiv.io.m_axis_dout_tready := false.B
        out_valid := true.B
        in_ready := true.B
        state_s := idle
      }
    }
  }

  val state_u = RegInit(idle)

  switch(state_u) {
    is(idle) {
      when(io.in.valid && MDUOpType.isDiv(io.in.bits.ctrl.fuOpType) && MDUOpType.isDivSign(io.in.bits.ctrl.fuOpType)) {
        out_valid := false.B
        udiv.io.s_axis_dividend_tvalid := true.B
        udiv.io.s_axis_divisor_tvalid := true.B
        in_ready := false.B
        state_u := put
      }
    }
    is(put) {
      when(udiv.io.s_axis_dividend_tready && udiv.io.s_axis_divisor_tready) {
        udiv.io.s_axis_dividend_tvalid := false.B
        udiv.io.s_axis_divisor_tvalid := false.B
        udiv.io.m_axis_dout_tready := true.B
        state_u := waiting
      }
    }
    is(waiting) {
      when(udiv.io.m_axis_dout_tvalid) {
        divu := udiv.io.m_axis_dout_tdata(63, 32)
        modu := udiv.io.m_axis_dout_tdata(31, 0)
        udiv.io.m_axis_dout_tready := false.B
        out_valid := true.B
        in_ready := true.B
        state_u := idle
      }
    }
  }

  io.in.ready := in_ready
  io.out.valid := out_valid

  io.out.bits.data := MuxLookup(io.in.bits.ctrl.fuOpType, 0.U)(Seq(
    MDUOpType.div -> div,
    MDUOpType.divu -> divu,
    MDUOpType.mul -> ((io.in.bits.src1.asSInt * io.in.bits.src2.asSInt).asUInt),
    MDUOpType.mulh -> ((io.in.bits.src1.asSInt * io.in.bits.src2.asSInt) >> 32).asUInt,
    MDUOpType.mulhu -> ((io.in.bits.src1 * io.in.bits.src2) >> 32).asUInt,
    MDUOpType.mod -> mod,
    MDUOpType.modu -> modu
  ))
} 