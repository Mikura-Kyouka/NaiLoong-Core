package core

import core._
import chisel3._
import chisel3.util._

object InstType {
  val normal = 0.U(3.W)
  val load = 1.U(3.W)
  val store = 2.U(3.W)
  val branch = 3.U(3.W)
}

class WriteBackEntry extends Bundle {
  val idx = UInt(6.W)
  val exception = Bool()
  val excpCode = UInt(4.W)
}

class CommitInfo extends Bundle {
  val pc = UInt(32.W)
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val preg = UInt(6.W)
  val opreg = UInt(6.W)
  val instType = UInt(3.W)
  val exception = Bool()
  val excpCode = UInt(4.W)
}

class RobEntry extends Bundle {
  val valid = Bool()
  val complete = Bool()
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val preg = UInt(6.W)
  val opreg = UInt(6.W)
  val pc = UInt(32.W)
  val exception = Bool()
  val excpCode = UInt(4.W)
  val instType = UInt(3.W)
  val checkpoint = UInt(6.W)
}

class ROB extends Module {

  val ROB_SIZE = 64

  val io = IO(new Bundle {
    val disp_valid = Input(Vec(4, Bool()))
    val disp_info = Input(Vec(4, new DispatchEntry))
    val rob_alloc_ready = Output(Bool())

    val wb_valid = Input(Vec(4, Bool()))
    val wb_info = Input(Vec(4, new WriteBackEntry))

    val flush_valid = Input(Bool())
    val flush_index = Input(UInt(log2Ceil(ROB_SIZE).W))

    val storeCommitAck = Input(Bool())
    val storeCommitReq = Output(Bool())

    val commit_valid = Output(Vec(4, Bool()))
    val commit_info = Output(Vec(4, new CommitInfo))
  })

  val robEntries = RegInit(
    VecInit(Seq.fill(ROB_SIZE)(0.U.asTypeOf(new RobEntry)))
  )

  val head = RegInit(0.U(log2Ceil(ROB_SIZE).W))
  val tail = RegInit(0.U(log2Ceil(ROB_SIZE).W))
  val count = RegInit(0.U((log2Ceil(ROB_SIZE) + 1).W))

  val numDispatch = PopCount(io.disp_valid)
  io.rob_alloc_ready := (ROB_SIZE.U - count) >= numDispatch

  val wrapIndex = (idx: UInt) => idx % ROB_SIZE.U

  val allocated = RegInit(0.U(2.W))
  allocated := 0.U

  when(io.rob_alloc_ready && numDispatch =/= 0.U) {
    for (i <- 0 until 4) {
      when(io.disp_valid(i)) {
        val allocIdx = wrapIndex(tail + allocated)
        robEntries(allocIdx) := {
          val entry = Wire(new RobEntry)
          entry.valid := true.B
          entry.complete := false.B
          entry.rj := io.disp_info(i).rj
          entry.rk := io.disp_info(i).rk
          entry.rd := io.disp_info(i).rd
          entry.preg := io.disp_info(i).preg
          entry.opreg := io.disp_info(i).opreg
          entry.pc := io.disp_info(i).pc
          entry.exception := false.B
          entry.excpCode := 0.U
          entry.instType := io.disp_info(i).instType
          entry.checkpoint := io.disp_info(i).checkpoint
          entry
        }
        allocated := allocated + 1.U
      }
    }
    tail := wrapIndex(tail + numDispatch)
    count := count + numDispatch
  }

  for (i <- 0 until 4) {
    when(io.wb_valid(i)) {
      val idx = io.wb_info(i).idx
      robEntries(idx).complete := true.B
      robEntries(idx).exception := io.wb_info(i).exception
      robEntries(idx).excpCode := io.wb_info(i).excpCode
    }
  }

  val commit_valid_vec = Wire(Vec(4, Bool()))
  val commit_info_vec = Wire(Vec(4, new CommitInfo))

  for (i <- 0 until 4) {
    val idx = wrapIndex(head + i.U)
    val entry = robEntries(idx)
    val canCommit = entry.valid && entry.complete
    commit_valid_vec(i) := canCommit

    commit_info_vec(i).pc := entry.pc
    commit_info_vec(i).rj := entry.rj
    commit_info_vec(i).rk := entry.rk
    commit_info_vec(i).rd := entry.rd
    commit_info_vec(i).preg := entry.preg
    commit_info_vec(i).opreg := entry.opreg
    commit_info_vec(i).instType := entry.instType
    commit_info_vec(i).exception := entry.exception
    commit_info_vec(i).excpCode := entry.excpCode
  }

  val commit_valid_ordered = Wire(Vec(4, Bool()))
  commit_valid_ordered(0) := commit_valid_vec(0)
  for (i <- 1 until 4) {
    commit_valid_ordered(i) := commit_valid_vec(i) && commit_valid_ordered(
      i - 1
    )
  }

  io.storeCommitReq := false.B

  when(commit_valid_ordered(0) && robEntries(head).instType === InstType.store) {
    io.storeCommitReq := true.B
  }

  io.commit_valid := commit_valid_ordered
  io.commit_info := commit_info_vec
}
