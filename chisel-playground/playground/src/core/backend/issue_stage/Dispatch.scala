package core

import chisel3._
import chisel3.util._

import IssueConfig._

class Dispatch extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(4, Output(new dispatch_in_info)))) // TODO: FETCH_WIDTH
    val out = Vec(ISSUE_WIDTH, Decoupled(new dispatch_out_info))
  })

  for (q <- io.out) {
    q.bits.inst_cnt := 0.U
    for(i <- 0 until ISSUE_WIDTH) {
      q.bits.inst_vec(i) := DontCare
    }
  }
  // FIXME: ISSUE_WIDTH != FETCH_WIDTH
  io.in.ready := !io.in.valid || (io.out(0).ready && io.out(1).ready && io.out(2).ready && io.out(3).ready) // TODO
  for (i <- 0 until ISSUE_WIDTH) { io.out(i).valid := io.in.valid }

  for(i <- 0 until 4) { // TODO: FETCH_WIDTH
    val inst = io.in.bits(i)

    val alu_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).op === FuType.alu, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))
    // reduceOption: combine all elements with addition
    // getOrElse: provides a default value (0) if no elements exist
    
    val muldiv_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).op === FuType.mdu, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))
    
    val loadstore_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).op === FuType.lsu, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))

    val branch_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).op === FuType.bru, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))

    // 根据指令类型分发
    when(inst.op === FuType.alu) { // ALU 指令
      val cnt = Cat(0.U(1.W), alu_cnt_before >> 1)
      when((alu_cnt_before % 2.U) === 0.U) {
        io.out(0).bits.inst_vec(cnt) := inst
        io.out(0).bits.inst_cnt := cnt + 1.U
      } .otherwise {
        io.out(1).bits.inst_vec(cnt) := inst
        io.out(1).bits.inst_cnt := cnt + 1.U
      }
    } .elsewhen(inst.op === FuType.mdu) { // MUL/DIV 指令
      io.out(2).bits.inst_vec(muldiv_cnt_before) := inst
      io.out(2).bits.inst_cnt := muldiv_cnt_before + 1.U
    } .elsewhen(inst.op === FuType.lsu) { // Load/Store 指令
      io.out(3).bits.inst_vec(loadstore_cnt_before) := inst
      io.out(3).bits.inst_cnt := loadstore_cnt_before + 1.U
    }.elsewhen(inst.op === FuType.bru) { // Branch 指令
      io.out(4).bits.inst_vec(loadstore_cnt_before) := inst
      io.out(4).bits.inst_cnt := branch_cnt_before + 1.U
    }
  }
}