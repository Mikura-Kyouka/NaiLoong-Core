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
  def isMul(op: UInt) = !op(2)
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
    val markIntrpt = Input(Bool())
    val flush = Input(Bool())
  })
  val mdu = Module(new MDU)
  mdu.io := DontCare
  mdu.io.in.bits.src1 := io.in.bits.src1
  mdu.io.in.bits.src2 := Mux(io.in.bits.ctrl.src2Type === 1.U, io.in.bits.imm, io.in.bits.src2)
  mdu.io.in.bits.func := io.in.bits.ctrl.fuOpType
  io.out.bits.pc := io.in.bits.pc
  io.out.bits.data := mdu.io.out.bits
  io.out.bits.robIdx := io.in.bits.robIdx
  io.out.bits.preg := io.in.bits.preg
  
  mdu.io.in.valid := io.in.valid
  mdu.io.out.ready := io.out.ready
  io.out.bits.redirect.actuallyTaken := io.in.bits.redirect.actuallyTaken
  io.out.bits.redirect.actuallyTarget := io.in.bits.pc + 4.U
  io.out.bits.redirect.predictTaken := io.in.bits.redirect.predictTaken
  io.out.bits.redirect.predictTarget := io.in.bits.redirect.predictTarget
  io.out.bits.redirect.rtype := io.in.bits.redirect.rtype
  io.out.bits.redirect.valid := io.in.bits.redirect.valid
  io.out.bits.timer64 := DontCare  // TODO
  io.out.bits.tlbInfo := DontCare
  io.out.bits.vaddr := DontCare
  
  val in_ready = RegInit(true.B)
  val out_valid = RegInit(false.B)

  io.in.ready := in_ready && (!(io.in.valid && io.in.bits.valid) || io.out.fire)
  io.out.valid := out_valid && io.in.bits.valid
  
  val idle :: put :: waiting :: get :: Nil = Enum(4)
  val div = RegInit(0.U(32.W))
  val divu = RegInit(0.U(32.W))
  val mod = RegInit(0.U(32.W))
  val modu = RegInit(0.U(32.W))

  if (GenCtrl.USE_SIMU) {
    val sdiv = Module(new SignedDivider)
    val udiv = Module(new UnsignedDivider)

    sdiv.io.aclk := clock
    sdiv.io.aresetn := !io.flush
    sdiv.io.s_axis_dividend_tdata := io.in.bits.src1.asSInt
    sdiv.io.s_axis_divisor_tdata  := io.in.bits.src2.asSInt
    sdiv.io.s_axis_dividend_tvalid := false.B
    sdiv.io.s_axis_divisor_tvalid  := false.B
    sdiv.io.m_axis_dout_tready := false.B

    udiv.io.aclk := clock
    udiv.io.aresetn := !io.flush
    udiv.io.s_axis_dividend_tdata := io.in.bits.src1
    udiv.io.s_axis_divisor_tdata  := io.in.bits.src2
    udiv.io.s_axis_dividend_tvalid := false.B
    udiv.io.s_axis_divisor_tvalid  := false.B
    udiv.io.m_axis_dout_tready := false.B

    val state_s = RegInit(idle)

    val sdiv_dividend_tvalid = RegInit(false.B)
    val sdiv_divisor_tvalid = RegInit(false.B)
    val sdiv_dout_tready = RegInit(false.B)

    switch(state_s) {
      is(idle) {
        when(io.in.valid && io.in.bits.valid && MDUOpType.isDiv(io.in.bits.ctrl.fuOpType) && MDUOpType.isDivSign(io.in.bits.ctrl.fuOpType)) {
          out_valid := false.B
          sdiv_dividend_tvalid := true.B
          sdiv_divisor_tvalid := true.B
          in_ready := false.B
          state_s := put
        }.elsewhen(io.in.valid && io.in.bits.valid && MDUOpType.isMul(io.in.bits.ctrl.fuOpType)) {
          out_valid := true.B
          in_ready := false.B
          state_s := get
        }
      }
      is(put) {
        when(sdiv.io.s_axis_dividend_tready && sdiv.io.s_axis_divisor_tready) {
          sdiv_dividend_tvalid := false.B
          sdiv_divisor_tvalid := false.B
          sdiv_dout_tready := true.B
          state_s := waiting
        }
      }
      is(waiting) {
        when(sdiv.io.m_axis_dout_tvalid) {
          div := sdiv.io.m_axis_dout_tdata(63, 32)
          mod := sdiv.io.m_axis_dout_tdata(31, 0)
          sdiv_dout_tready := false.B
          out_valid := true.B
          in_ready := true.B
          state_s := get
        }
      }
      is(get) {
        when(io.out.fire) {
          out_valid := false.B
          in_ready := true.B
          state_s := idle
        }
      }
    }

    sdiv.io.s_axis_dividend_tvalid := sdiv_dividend_tvalid
    sdiv.io.s_axis_divisor_tvalid := sdiv_divisor_tvalid
    sdiv.io.m_axis_dout_tready := sdiv_dout_tready

    val state_u = RegInit(idle)

    val udiv_dividend_tvalid = RegInit(false.B)
    val udiv_divisor_tvalid = RegInit(false.B)
    val udiv_dout_tready = RegInit(false.B)

    switch(state_u) {
      is(idle) {
        when(io.in.valid && io.in.bits.valid && MDUOpType.isDiv(io.in.bits.ctrl.fuOpType) && !MDUOpType.isDivSign(io.in.bits.ctrl.fuOpType)) {
          out_valid := false.B
          udiv_dividend_tvalid := true.B
          udiv_divisor_tvalid := true.B
          in_ready := false.B
          state_u := put
        }.elsewhen(io.in.valid && io.in.bits.valid && MDUOpType.isMul(io.in.bits.ctrl.fuOpType)) {
          out_valid := true.B
          in_ready := false.B
          state_u := get
        }
      }
      is(put) {
        when(udiv.io.s_axis_dividend_tready && udiv.io.s_axis_divisor_tready) {
          udiv_dividend_tvalid := false.B
          udiv_divisor_tvalid := false.B
          udiv_dout_tready := true.B
          state_u := waiting
        }
      }
      is(waiting) {
        when(udiv.io.m_axis_dout_tvalid) {
          divu := udiv.io.m_axis_dout_tdata(63, 32)
          modu := udiv.io.m_axis_dout_tdata(31, 0)
          udiv_dout_tready := false.B
          out_valid := true.B
          state_u := get
        }
      }
      is(get) {
        when(io.out.fire) {
          out_valid := false.B
          in_ready := true.B
          state_u := idle
        }
      }
    }

    udiv.io.s_axis_dividend_tvalid := udiv_dividend_tvalid
    udiv.io.s_axis_divisor_tvalid := udiv_divisor_tvalid
    udiv.io.m_axis_dout_tready := udiv_dout_tready

    when(io.flush) {
      state_s := idle
      state_u := idle
      out_valid := false.B
      in_ready := true.B
      sdiv_dividend_tvalid := false.B
      sdiv_divisor_tvalid := false.B
      sdiv_dout_tready := false.B
      udiv_dividend_tvalid := false.B
      udiv_divisor_tvalid := false.B
      udiv_dout_tready := false.B
    }

    io.out.bits.data := MuxLookup(io.in.bits.ctrl.fuOpType, 0.U)(Seq(
      MDUOpType.div -> div,
      MDUOpType.divu -> divu,
      MDUOpType.mul -> ((io.in.bits.src1.asSInt * io.in.bits.src2.asSInt).asUInt),
      MDUOpType.mulh -> ((io.in.bits.src1.asSInt * io.in.bits.src2.asSInt) >> 32).asUInt,
      MDUOpType.mulhu -> ((io.in.bits.src1 * io.in.bits.src2) >> 32).asUInt,
      MDUOpType.mod -> mod,
      MDUOpType.modu -> modu
    ))

  } else {
    
    val sdiv = Module(new SignedDividerBlackBox)
    val udiv = Module(new UnsignedDividerBlackBox)

    sdiv.io.aclk := clock
    sdiv.io.aresetn := !(reset.asBool || io.flush)
    sdiv.io.s_axis_dividend_tdata := io.in.bits.src1.asSInt
    sdiv.io.s_axis_divisor_tdata  := io.in.bits.src2.asSInt
    sdiv.io.s_axis_dividend_tvalid := false.B
    sdiv.io.s_axis_divisor_tvalid  := false.B
    sdiv.io.m_axis_dout_tready := false.B

    udiv.io.aclk := clock
    udiv.io.aresetn := !(reset.asBool || io.flush)
    udiv.io.s_axis_dividend_tdata := io.in.bits.src1
    udiv.io.s_axis_divisor_tdata  := io.in.bits.src2
    udiv.io.s_axis_dividend_tvalid := false.B
    udiv.io.s_axis_divisor_tvalid  := false.B
    udiv.io.m_axis_dout_tready := false.B

    val state_s = RegInit(idle)

    val sdiv_dividend_tvalid = RegInit(false.B)
    val sdiv_divisor_tvalid = RegInit(false.B)
    val sdiv_dout_tready = RegInit(false.B)

    switch(state_s) {
      is(idle) {
        when(io.in.valid && io.in.bits.valid && MDUOpType.isDiv(io.in.bits.ctrl.fuOpType) && MDUOpType.isDivSign(io.in.bits.ctrl.fuOpType)) {
          out_valid := false.B
          sdiv_dividend_tvalid := true.B
          sdiv_divisor_tvalid := true.B
          in_ready := false.B
          state_s := put
        }.elsewhen(io.in.valid && io.in.bits.valid && MDUOpType.isMul(io.in.bits.ctrl.fuOpType)) {
          out_valid := true.B
          in_ready := false.B
          state_s := get
        }
      }
      is(put) {
        when(sdiv.io.s_axis_dividend_tready && sdiv.io.s_axis_divisor_tready) {
          sdiv_dividend_tvalid := false.B
          sdiv_divisor_tvalid := false.B
          sdiv_dout_tready := true.B
          state_s := waiting
        }
      }
      is(waiting) {
        when(sdiv.io.m_axis_dout_tvalid) {
          div := sdiv.io.m_axis_dout_tdata(63, 32)
          mod := sdiv.io.m_axis_dout_tdata(31, 0)
          sdiv_dout_tready := false.B
          out_valid := true.B
          in_ready := true.B
          state_s := get
        }
      }
      is(get) {
        when(io.out.fire) {
          out_valid := false.B
          in_ready := true.B
          state_s := idle
        }
      }
    }

    sdiv.io.s_axis_dividend_tvalid := sdiv_dividend_tvalid
    sdiv.io.s_axis_divisor_tvalid := sdiv_divisor_tvalid
    sdiv.io.m_axis_dout_tready := sdiv_dout_tready

    val state_u = RegInit(idle)

    val udiv_dividend_tvalid = RegInit(false.B)
    val udiv_divisor_tvalid = RegInit(false.B)
    val udiv_dout_tready = RegInit(false.B)

    switch(state_u) {
      is(idle) {
        when(io.in.valid && io.in.bits.valid && MDUOpType.isDiv(io.in.bits.ctrl.fuOpType) && !MDUOpType.isDivSign(io.in.bits.ctrl.fuOpType)) {
          out_valid := false.B
          udiv_dividend_tvalid := true.B
          udiv_divisor_tvalid := true.B
          in_ready := false.B
          state_u := put
        }.elsewhen(io.in.valid && io.in.bits.valid && MDUOpType.isMul(io.in.bits.ctrl.fuOpType)) {
          out_valid := true.B
          in_ready := false.B
          state_u := get
        }
      }
      is(put) {
        when(udiv.io.s_axis_dividend_tready && udiv.io.s_axis_divisor_tready) {
          udiv_dividend_tvalid := false.B
          udiv_divisor_tvalid := false.B
          udiv_dout_tready := true.B
          state_u := waiting
        }
      }
      is(waiting) {
        when(udiv.io.m_axis_dout_tvalid) {
          divu := udiv.io.m_axis_dout_tdata(63, 32)
          modu := udiv.io.m_axis_dout_tdata(31, 0)
          udiv_dout_tready := false.B
          out_valid := true.B
          in_ready := true.B
          state_u := get
        }
      }
      is(get) {
        when(io.out.fire) {
          out_valid := false.B
          in_ready := true.B
          state_u := idle
        }
      }
    }

    udiv.io.s_axis_dividend_tvalid := udiv_dividend_tvalid
    udiv.io.s_axis_divisor_tvalid := udiv_divisor_tvalid
    udiv.io.m_axis_dout_tready := udiv_dout_tready

    when(io.flush) {
      state_s := idle
      state_u := idle
      out_valid := false.B
      in_ready := true.B
      sdiv_dividend_tvalid := false.B
      sdiv_divisor_tvalid := false.B
      sdiv_dout_tready := false.B
      udiv_dividend_tvalid := false.B
      udiv_divisor_tvalid := false.B
      udiv_dout_tready := false.B
    }

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
  
  // for difftest
  io.out.bits.paddr := DontCare
  io.out.bits.wdata := DontCare
  io.out.bits.optype := DontCare
  io.out.bits.fuType := io.in.bits.ctrl.fuType
  io.out.bits.csrNewData := DontCare
  io.out.bits.exceptionVec := Cat(0.U(15.W), io.markIntrpt)
  io.out.bits.failsc := false.B // MDU does not handle sc
} 