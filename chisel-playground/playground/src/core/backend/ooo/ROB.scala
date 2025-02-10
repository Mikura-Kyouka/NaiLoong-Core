package core

import chisel3._
import chisel3.util._

object InstType {
  val normal = 0.U(3.W)
  val load = 1.U(3.W)
  val store = 2.U(3.W)
  val branch = 3.U(3.W)
}

class DispatchEntry extends Bundle {
  val pc = UInt(32.W)
  val areg = UInt(5.W)
  val preg = UInt(6.W)
  val opreg = UInt(6.W)
  val instType = UInt(3.W)
  val checkpoint = UInt(6.W)
}

class WriteBackEntry extends Bundle {
  val idx = UInt(6.W)
  val exception = Bool()
  val excpCode = UInt(4.W)
}

class CommitInfo extends Bundle {
  val pc = UInt(32.W)
  val areg = UInt(5.W)
  val preg = UInt(6.W)
  val opreg = UInt(6.W)
  val instType = UInt(3.W)
  val exception = Bool()
  val excpCode = UInt(4.W)
}

class RobEntry extends Bundle {
  val valid = Bool()
  val complete = Bool()
  val areg = UInt(5.W)
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

    // flush_valid 为高时表示需要进行恢复，flush_index 表示从哪个 ROB 条目（含该条目）开始 flush
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

  // 循环队列指针：
  val head = RegInit(0.U(log2Ceil(ROB_SIZE).W))
  val tail = RegInit(0.U(log2Ceil(ROB_SIZE).W))
  val count = RegInit(0.U((log2Ceil(ROB_SIZE) + 1).W))

  // 分配接口：检查是否有足够空槽
  io.rob_alloc_ready := (ROB_SIZE.U - count) >= PopCount(io.disp_valid)

  // Dispatch（分配）逻辑
  // 计算每个i对应的前缀和（前面有多少个有效请求）
  val prefixSum = Wire(Vec(4, UInt(2.W)))
  prefixSum(0) := 0.U
  for (i <- 1 until 4) {
    prefixSum(i) := prefixSum(i - 1) + io.disp_valid(i - 1)
  }

  // 分配逻辑
  when(io.rob_alloc_ready && (numDispatch =/= 0.U)) {
    var allocated = 0.U(2.W)
    for (i <- 0 until 4) {
      when(io.disp_valid(i)) {
        val allocIdx =
          (tail + prefixSum(i) + allocated)(log2Ceil(ROB_SIZE) - 1, 0)
        robEntries(allocIdx) := {
          val entry = Wire(new RobEntry)
          entry.valid := true.B
          entry.complete := false.B
          entry.areg := io.disp_info(i).areg
          entry.preg := io.disp_info(i).preg
          entry.opreg := io.disp_info(i).opreg
          entry.pc := io.disp_info(i).pc
          entry.exception := false.B
          entry.excpCode := 0.U
          entry.instType := io.disp_info(i).instType
          entry.checkpoint := io.disp_info(i).checkpoint
          entry
        }
        allocated += 1.U
      }
    }
    tail := tail + numDispatch
    count := count + numDispatch
  }

  // WriteBack（写回）逻辑
  for (i <- 0 until 4) {
    when(io.wb_valid(i)) {
      val idx = io.wb_info(i).idx
      robEntries(idx).complete := true.B
      robEntries(idx).exception := io.wb_info(i).exception
      robEntries(idx).excpCode := io.wb_info(i).excpCode
    }
  }

  // Commit（提交）逻辑
  val commit_valid_vec = Wire(Vec(4, Bool()))
  val commit_info_vec = Wire(Vec(4, new CommitInfo))
  for (i <- 0 until 4) {
    val idx = (head + i.U)(log2Ceil(ROB_SIZE) - 1, 0)
    val entry = robEntries(idx)
    val canCommit = entry.valid && entry.complete
    val isStore = (entry.instType === InstType.store)
    val storeOk = Mux(isStore, io.storeCommitAck, true.B)
    commit_valid_vec(i) := canCommit && storeOk
    commit_info_vec(i).pc := entry.pc
    commit_info_vec(i).areg := entry.areg
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

  io.commit_valid := commit_valid_ordered
  io.commit_info := commit_info_vec

  val commitCount = Wire(UInt(3.W))
  when(!commit_valid_ordered(0)) {
    commitCount := 0.U
  }.elsewhen(!commit_valid_ordered(1)) {
    commitCount := 1.U
  }.elsewhen(!commit_valid_ordered(2)) {
    commitCount := 2.U
  }.elsewhen(!commit_valid_ordered(3)) {
    commitCount := 3.U
  }.otherwise {
    commitCount := 4.U
  }

  when(commitCount > 0.U) {
    for (i <- 0 until 4) {
      when(i.U < commitCount) {
        val idx = (head + i.U)(log2Ceil(ROB_SIZE) - 1, 0)
        robEntries(idx).valid := false.B
      }
    }
    head := head + commitCount
    count := count - commitCount
  }

  io.storeCommitReq := (robEntries(head).instType === InstType.store) &&
    robEntries(head).valid &&
    robEntries(head).complete &&
    !io.storeCommitAck

  when(io.flush_valid) {
    val flushIdx = io.flush_index
    val (newCount, invalidateRange) =
      if (flushIdx >= head) (flushIdx - head, head until flushIdx)
      else
        (
          flushIdx + ROB_SIZE.U - head,
          head until ROB_SIZE.U ++ 0.U until flushIdx
        )

    invalidateRange.foreach { idx =>
      robEntries(idx).valid := false.B
    }

    tail := flushIdx
    head := Mux(flushIdx === 0.U, (ROB_SIZE - 1).U, flushIdx - 1.U)
    count := newCount
  }
}
