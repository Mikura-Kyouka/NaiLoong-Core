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

    val reg = RegInit(0.U.asTypeOf(left.bits))
    when(left.valid && right.ready) {
      reg := left.bits
    }
    state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(left.valid && right.ready, s_in, s_idle),
      s_in -> Mux(rightOutFire, Mux(left.valid && right.ready, s_in, s_idle), s_in)
    ))

    when(state === s_in) {
      right.bits := reg
    }.otherwise{
      right.bits := reg
      // right.bits match {
      //   case b: IDU2EXU => b.regW := false.B
      //   case b: EXU2LSU => b.regW := false.B
      //   case _ => // 如果不是 IDU2EXU 或 EXU2LSU，不做任何操作
      // }
      // right.bits match {
      //   case b: EXU2LSU => b.needMem := false.B
      //   case _ => 
      // }
    }

    val valid = RegInit(false.B)
    when(rightOutFire) { valid := false.B } // already excepted, right.valid := false
    when(left.valid && right.ready) { valid := true.B } // in.fire
    when(isFlush) { valid := false.B }
    when(~left.valid) { valid := false.B }

    left.ready := right.ready
    right.valid := valid
  }
}

// object PipelineConnect2 {
//   def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool) = {
//     val valid = RegInit(false.B)
//     when (rightOutFire) { valid := false.B }
//     when (left.valid && right.ready) { valid := true.B }
//     when (isFlush) { valid := false.B }

//     left.ready := right.ready
//     right.bits := RegEnable(left.bits, left.valid && right.ready)
//     right.valid := valid //&& !isFlush
//   }
// }

object conflictWithStage {
  def conflict(rs: UInt, rd: UInt) = (rs === rd && rd =/= 0.U)
  def apply[T <: Data](rs1: UInt, rs2: UInt, rd: UInt, regW: Bool) = {
    (conflict(rs1, rd) || conflict(rs2, rd)) && regW
  }
}