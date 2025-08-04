package utils

import chisel3._
import chisel3.util._

object PipelineConnect {
  def apply[T <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool
  ) = {
    val s_idle :: s_in  :: Nil = Enum(2)
    val state = RegInit(s_idle)
    val valid = RegInit(false.B)

    val reg = RegInit(0.U.asTypeOf(left.bits))
    when(left.valid && right.ready) {
      reg := left.bits
    }
    state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(left.valid && right.ready, s_in, s_idle),
      s_in -> Mux(rightOutFire, Mux(left.valid && right.ready, s_in, s_idle), s_in)
    ))
    
    right.bits := reg

    when(rightOutFire) { valid := false.B } // 被下游接收, 当前数据in.valid所指示的数据无效, 拉低, 否则一直是高
    when(left.valid && right.ready) { valid := true.B } // 握手之后, 表示当前进来的数据有效, right.valid高, 且晚于握手一拍
    when(isFlush) { valid := false.B }

    left.ready := right.ready
    right.valid := valid && !isFlush
  }
}

object PipelineConnect2 {
  def apply[T <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool
  ) = {
    val s_idle :: s_in  :: Nil = Enum(2)
    val state = RegInit(s_idle)

    val reg = RegInit(0.U.asTypeOf(left.bits))

    val valid = RegInit(false.B)
    val valid_reg = RegInit(false.B)

    state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(left.valid && right.ready, s_in, s_idle),
      s_in -> Mux(rightOutFire, Mux(left.valid && right.ready, s_in, s_idle), s_in)
    ))

    right.bits := reg
  
    // when(left.valid) { valid_reg := true.B }
    // when(valid_reg && right.ready) { valid_reg := false.B }
    // when(isFlush) { valid_reg := false.B }

    // when(valid_reg && right.ready) { valid := true.B } // in.fire
    // when(isFlush) { valid := false.B }

    when((left.valid || valid_reg) && right.ready) {
      valid := true.B 
      reg := left.bits
    }.otherwise {
      valid := false.B
    }

    when((left.valid || valid_reg) && right.ready) {
      valid_reg := false.B
    }.elsewhen(left.valid) {
      valid_reg := true.B
    }

    when(isFlush) {
      valid_reg := false.B
      valid := false.B
    }

    left.ready := right.ready
    right.valid := valid
  }
}

object PipelineConnect3 {
  def apply[T <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool
  ) = {
    val s_idle :: s_in  :: Nil = Enum(2)
    val state = RegInit(s_idle)
    val valid = RegInit(false.B)

    val reg = RegInit(0.U.asTypeOf(left.bits))
    when(left.valid) {
      reg := left.bits
    }
    state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(left.valid && right.ready, s_in, s_idle),
      s_in -> Mux(rightOutFire, Mux(left.valid && right.ready, s_in, s_idle), s_in)
    ))
    
    right.bits := reg

    when(rightOutFire) { valid := false.B } // 被下游接收, 当前数据in.valid所指示的数据无效, 拉低, 否则一直是高
    when(left.valid && right.ready) { valid := true.B } // 握手之后, 表示当前进来的数据有效, right.valid高, 且晚于握手一拍
    when(isFlush) { valid := false.B }

    left.ready := right.ready
    right.valid := valid
  }
}

object conflictWithStage {
  def conflict(rs: UInt, rd: UInt) = (rs === rd && rd =/= 0.U)
  def apply[T <: Data](rs1: UInt, rs2: UInt, rd: UInt, regW: Bool) = {
    (conflict(rs1, rd) || conflict(rs2, rd)) && regW
  }
}