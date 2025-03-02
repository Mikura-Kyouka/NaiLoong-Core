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
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val preg = UInt(7.W)
  val opreg = UInt(7.W)
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
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val preg = UInt(7.W)
  val opreg = UInt(7.W)
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
  val preg = UInt(7.W)
  val opreg = UInt(7.W)
  val pc = UInt(32.W)
  val exception = Bool()
  val excpCode = UInt(4.W)
  val instType = UInt(3.W)
  val checkpoint = UInt(6.W)
}

class RobCommitBundle extends Bundle {
  val valid = Bool()
  val old_preg = UInt(RegConfig.PHYS_REG_BITS.W)
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

    val rob_commit = Output(Vec(4, new RobCommitBundle))
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

  when(io.rob_alloc_ready && numDispatch =/= 0.U) {
    val allocated = PopCount(io.disp_valid)
    
    val (indices, _) = io.disp_valid.zipWithIndex.foldLeft((Seq.empty[UInt], 0.U)) {
    case ((accIndices, cnt), (valid, i)) =>
      val newCnt = cnt + valid.asUInt
      val newIndices = if (i == 0) Seq.empty else accIndices
      val index = Mux(valid, wrapIndex(tail + cnt), 0.U)
      (newIndices :+ index, newCnt)
    }

    for (i <- 0 until 4) {
      when(io.disp_valid(i)) {
        robEntries(indices(i)) := {
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
      }
    }
    tail := wrapIndex(tail + allocated)
    count := count + allocated
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
  val commit_old_preg_valid = VecInit(robEntries.map(e => e.valid && e.complete))

  for (i <- 0 until 4) {
    val idx = wrapIndex(head + i.U)
    val entry = robEntries(idx)
    val canCommit = entry.valid && entry.complete
    commit_valid_vec(i) := entry.valid && entry.complete && 
                      (entry.instType =/= InstType.store || io.storeCommitAck)

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


  val has_exception = commit_info_vec.map(_.exception).reduce(_ || _)

  val commit_valid_ordered = Wire(Vec(4, Bool()))

  commit_valid_ordered(0) := commit_valid_vec(0)
  for (i <- 1 until 4) {
    commit_valid_ordered(i) := commit_valid_vec(i) && 
                              commit_valid_ordered(i-1) && 
                              robEntries(wrapIndex(head + i.U)).instType =/= InstType.store
  }

  // 物理寄存器回收队列
  val RecycleQueueDepth = 8
  val recycleQueue = Module(new Queue(
    gen = Vec(4, new RobCommitBundle),
    entries = RecycleQueueDepth,
    pipe = true,
    hasFlush = true
  ))
  recycleQueue.io.flush.get := io.flush_valid

  recycleQueue.io.enq.valid := io.commit_valid.asUInt.orR
  recycleQueue.io.enq.bits := VecInit(io.commit_info.zip(io.commit_valid).map {
    case (info, valid) => {
      val entry = Wire(new RobCommitBundle)
      entry.valid := valid
      entry.old_preg := info.opreg
      entry
    }
  })

  when(io.flush_valid) {
    head := io.flush_index
    tail := io.flush_index
    count := 0.U
    robEntries.foreach(_.valid := false.B)
  }

  val exceptionIndex = PriorityMux(
    commit_info_vec.map(_.exception).zipWithIndex.map { 
      case (e, idx) => (e, idx.U) 
    }
  )

  when(has_exception) {
    recycleQueue.io.enq.valid := (PriorityEncoder(commit_valid_ordered) + 1.U) >= exceptionIndex
  }
  
  val CommitValidPreMask = VecInit.tabulate(4) { i =>
    commit_valid_ordered(i) && (!has_exception || (i.U <= exceptionIndex))
  }

  val storeMask = VecInit(commit_info_vec.map(_.instType === InstType.store)).asUInt
  val pendingStore = (storeMask & CommitValidPreMask.asUInt).orR && !io.storeCommitAck
  val isBlockedByStore = VecInit.tabulate(4) { i =>
    (i > 0).B && pendingStore && (storeMask(i-1, 0).orR)
  }

  io.commit_valid := VecInit.tabulate(4) { i =>
    CommitValidPreMask(i) && !isBlockedByStore(i)
  }

  io.storeCommitReq := pendingStore
  io.commit_info := commit_info_vec

  val commit_num = PopCount(io.commit_valid)
  when(commit_num > 0.U && !has_exception) {
    head := wrapIndex(head + commit_num)
    count := count - commit_num
  }

  val rob_commit_reg = RegNext(recycleQueue.io.deq.bits)
  val rob_commit_valid_reg = RegNext(recycleQueue.io.deq.valid)
  io.rob_commit := Mux(
    rob_commit_valid_reg,
    rob_commit_reg,
    0.U.asTypeOf(Vec(4, new RobCommitBundle))
  )

  recycleQueue.io.deq.ready := true.B
}
