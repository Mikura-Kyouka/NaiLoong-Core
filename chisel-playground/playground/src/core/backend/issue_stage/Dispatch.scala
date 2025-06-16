package core

import chisel3._
import chisel3.util._

import IssueConfig._
import java.nio.channels.Pipe
import core.FuType.alu

class Dispatch extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(4, new PipelineConnectIO)))
    val out = Vec(ISSUE_WIDTH, Decoupled(new dispatch_out_info))
    val busy_info = Output(Vec(5, new busy_info))
  })

  val dispatch_dest = WireInit(VecInit(Seq.fill(4)(0.U(3.W)))) // 3 bits for 5 FuType
  val dispatch_vec_dest = WireInit(VecInit(Seq.fill(4)(0.U(3.W)))) 

  for (q <- io.out) {
    q.bits.inst_cnt := 0.U
    for(i <- 0 until ISSUE_WIDTH) {
      q.bits.inst_vec(i) := DontCare
    }
  }

  for (q <- io.busy_info) {
    q.valid := false.B
    q.preg := 0.U
  }

  // FIXME: paramiterize FETCH_WITDTH
  io.in.ready := io.out(0).ready && io.out(1).ready && io.out(2).ready && io.out(3).ready && io.out(4).ready
  
  for (i <- 0 until ISSUE_WIDTH) {
     io.out(i).valid := io.in.valid 
  } 

  val alu_dispatched = RegInit(false.B)
  when(io.out.map(_.valid).reduce(_ || _)) {
    alu_dispatched := false.B
  }

  for(i <- 0 until 4) { // TODO: FETCH_WIDTH
    val inst = io.in.bits(i)

    val alu_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).ctrl.fuType === FuType.alu && io.in.bits(j).valid, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))
    // reduceOption: combine all elements with addition
    // getOrElse: provides a default value (0) if no elements exist
    
    val muldiv_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).ctrl.fuType === FuType.mdu && io.in.bits(j).valid, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))
    
    val loadstore_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).ctrl.fuType === FuType.lsu && io.in.bits(j).valid, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))

    val branch_cnt_before = (0 until i).map { j =>
      Mux(io.in.bits(j).ctrl.fuType === FuType.bru && io.in.bits(j).valid, 1.U(3.W), 0.U(3.W))
    }.reduceOption(_ + _).getOrElse(0.U(3.W))

    // 根据指令类型分发
    when(inst.ctrl.fuType === FuType.alu) { // ALU 指令
      val cnt = Cat(0.U(1.W), alu_cnt_before >> 1)
      when((alu_cnt_before % 2.U) === 0.U) {
        io.out(0).bits.inst_vec(cnt) := inst
        io.out(0).bits.inst_cnt := cnt + 1.U
        io.busy_info(Mux(alu_dispatched, 1.U, 0.U)).preg := inst.preg
        io.busy_info(Mux(alu_dispatched, 1.U, 0.U)).valid := inst.preg =/= 0.U

        alu_dispatched := true.B
        // dispatch_dest(i) := 0.U
        // dispatch_vec_dest(i) := cnt
        // io.busy_info(0).preg := inst.preg
        // io.busy_info(0).valid := inst.preg =/= 0.U
      } .otherwise {
        io.out(1).bits.inst_vec(cnt) := inst
        io.out(1).bits.inst_cnt := cnt + 1.U
        io.busy_info(Mux(alu_dispatched, 1.U, 0.U)).preg := inst.preg
        io.busy_info(Mux(alu_dispatched, 1.U, 0.U)).valid := inst.preg =/= 0.U

        alu_dispatched := true.B
        // dispatch_dest(i) := 1.U
        // dispatch_vec_dest(i) := cnt
        // io.busy_info(1).preg := inst.preg
        // io.busy_info(1).valid := inst.preg =/= 0.U
      }
    } .elsewhen(inst.ctrl.fuType === FuType.mdu) { // MUL/DIV 指令
      io.out(2).bits.inst_vec(muldiv_cnt_before) := inst
      io.out(2).bits.inst_cnt := muldiv_cnt_before + 1.U
      // dispatch_dest(i) := 2.U
      // dispatch_vec_dest(i) := muldiv_cnt_before
      io.busy_info(2).preg := inst.preg
      io.busy_info(2).valid := inst.preg =/= 0.U
    } .elsewhen(inst.ctrl.fuType === FuType.lsu) { // Load/Store 指令
      io.out(3).bits.inst_vec(loadstore_cnt_before) := inst
      io.out(3).bits.inst_cnt := loadstore_cnt_before + 1.U
      // dispatch_dest(i) := 3.U
      // dispatch_vec_dest(i) := loadstore_cnt_before
      io.busy_info(3).preg := inst.preg
      io.busy_info(3).valid := inst.preg =/= 0.U
    }.elsewhen(inst.ctrl.fuType === FuType.bru) { // Branch 指令
      io.out(4).bits.inst_vec(branch_cnt_before) := inst
      io.out(4).bits.inst_cnt := branch_cnt_before + 1.U
      // dispatch_dest(i) := 4.U
      // dispatch_vec_dest(i) := branch_cnt_before
      io.busy_info(4).preg := inst.preg
      io.busy_info(4).valid := inst.preg =/= 0.U
    }
  }

  // // for busy reg
  // for (i <- 0 until 4) {
  //   io.busy_info(i).valid := io.out(dispatch_dest(i)).bits.inst_vec(dispatch_vec_dest(i)).preg =/= 0.U && 
  //                            io.out(dispatch_dest(i)).valid
  //   io.busy_info(i).preg  := io.out(dispatch_dest(i)).bits.inst_vec(dispatch_vec_dest(i)).preg
  // }

  /* 
   * 0 => ALU 
   * 1 => ALU
   * 2 => MUL/DIV
   * 3 => LOAD/STORE
   * 4 => BRANCH
   */
}