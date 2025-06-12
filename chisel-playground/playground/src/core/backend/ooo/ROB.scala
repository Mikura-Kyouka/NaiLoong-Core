package core

import chisel3._
import chisel3.util._
import core.LSUOpType.sh
import chisel3.util.experimental.decode.Minimizer
import core.LSUOpType.sw
import core.FuType.lsu
import core.LSUOpType.isStore

object RobConfig {
  val ROB_ENTRY_NUM = 64
  val ROB_INDEX_WIDTH = log2Ceil(ROB_ENTRY_NUM)
  val ROB_WRITEBACK_NUM = 5
}

class RobEntry extends Bundle {
  val valid      = Bool()
  val finished   = Bool()
  val pc         = UInt(32.W)
  val instr      = UInt(32.W)
  val exception  = Bool()
  val exceptionVec = UInt(16.W)
  val intrVec    = UInt(12.W)
  val rd         = UInt(5.W)
  val preg       = UInt(RegConfig.PHYS_REG_BITS.W)
  val use_preg   = Bool()
  val old_preg   = UInt(RegConfig.PHYS_REG_BITS.W)
  val rfWen      = Bool()
  val isBranch   = Bool()
  val isStore    = Bool()
  val brMispredict = Bool()
  val brTarget   = UInt(32.W)
  val fuType     = UInt(3.W)
  val result     = UInt(32.W) //FIXME
  val checkpoint = new Bundle {
    val valid = Bool()
    val id    = UInt(RegConfig.CHECKPOINT_DEPTH.W)
  }
  val inst_valid = Bool()
  val csrOp     = UInt(3.W)
  val csrNum    = UInt(14.W)
  val csrNewData = UInt(32.W)
  val eret = Bool() 

  // for load/store difftest
  val paddr      = UInt(32.W)
  val wdata      = UInt(32.W)
  val optype     = UInt(7.W)
  val timer64    = UInt(64.W)
}

// ROB分配接口
class RobAllocateIO extends Bundle {
  val allocReq     = Output(Bool())
  val allocCount   = Output(UInt(3.W))
  val allocEntries = Output(Vec(4, new RobEntry))
  val allocResp    = Input(Vec(4, UInt(RobConfig.ROB_INDEX_WIDTH.W)))
  val canAllocate  = Input(Bool())
}

// 写回接口
class RobWritebackInfo extends Bundle {
  val pc           = UInt(32.W)
  val robIdx       = UInt(RobConfig.ROB_INDEX_WIDTH.W)
  val exception    = Bool()
  val exceptionVec = UInt(16.W)
  val brMispredict = Bool()
  val brTarget     = UInt(32.W)
  val writeData    = UInt(32.W)
  val csrNewData   = UInt(32.W)

  // for load/store difftest
  val paddr       = UInt(32.W)
  val wdata       = UInt(32.W)
  val fuType      = UInt(3.W)
  val optype      = UInt(7.W)
  val timer64     = UInt(64.W)
}

class rtrBundle extends Bundle {
  val pc   = UInt(32.W)
  val dest = UInt(5.W)
  val preg = UInt(RegConfig.PHYS_REG_BITS.W)
  val data = UInt(32.W)
  val inst_valid = Bool()
  val use_preg = Bool()
  val csr_rstat = Bool()
  val csr_data = UInt(32.W)
  val excp = Bool()
  val timer64 = UInt(64.W)

  // for bpu
  val isBranch = Bool()
}

class BrMisPredInfo extends Bundle {
  val brMisPred = Valid(UInt(32.W))             // 分支预测错误信号
  val brMisPredTarget = UInt(32.W)               // 分支预测错误目标地址
  val brMisPredChkpt = UInt(RegConfig.CHECKPOINT_DEPTH.W) // 分支预测错误检查点ID
}

class LSCommitInfo extends Bundle {
  val paddr = UInt(32.W) // 物理地址
  val wdata = UInt(32.W) // 写入数据
  val optype = UInt(7.W) // 操作类型
}

// 提交接口
class RobCommit extends Bundle {
  val commit = Vec(4, Valid(new rtrBundle))
}

class RobIO extends Bundle {
  // 分配接口
  val allocate = Flipped(new RobAllocateIO)
  
  // 指令完成接口
  val writeback = Vec(5, Flipped(Valid(new RobWritebackInfo)))

  // ROB 和 LSU 关于 Store 指令的交互
  val RobLsuIn  = Flipped(DecoupledIO())
  val RobLsuOut = DecoupledIO()
  
  // 提交接口
  val commit = Output(new RobCommit)                       // 提交信息，用于释放物理寄存器
  val commitPC = Output(Vec(4, Valid(UInt(32.W))))         // 提交的PC
  val commitInstr = Output(Vec(4, Valid(UInt(32.W))))      // 提交的指令
  // for load/store difftest
  val commitLS = Output(Vec(4, Valid(new LSCommitInfo)))   // 提交的load/store信息
  val commitCSR = Vec(4, Valid(new csr_write_bundle))

  // 分支预测错误接口
  val brMisPredInfo = Output(new BrMisPredInfo)

  // 异常接口
  val exceptionInfo = Flipped(new csr_excp_bundle)

  val plv = Input(UInt(2.W)) // 当前特权级

  val flush = Output(Bool())
  val newPC = Output(UInt(32.W))
  
  // FIXME
  // val cdb = Vec(5, Flipped(Valid(new OOCommitIO)))
}

class Rob extends Module {
  val io = IO(new RobIO)
  
  // ROB条目数组
  val robEntries = RegInit(VecInit(Seq.fill(RobConfig.ROB_ENTRY_NUM)(0.U.asTypeOf(new RobEntry))))
  
  // 头尾指针
  val head = RegInit(0.U(RobConfig.ROB_INDEX_WIDTH.W))
  val tail = RegInit(0.U(RobConfig.ROB_INDEX_WIDTH.W))
  
  // 计算ROB中的条目数量
  val count = Mux(tail >= head, 
                tail - head, 
                (RobConfig.ROB_ENTRY_NUM.U - head) + tail)
  val empty = (head === tail) && !robEntries(head).valid
  val full = (count >= (RobConfig.ROB_ENTRY_NUM - 4).U)
  
  // 分配逻辑
  val canAlloc = !full && (RobConfig.ROB_ENTRY_NUM.U - count >= io.allocate.allocCount)
  io.allocate.canAllocate := canAlloc

  // 计算分配的索引
  for (i <- 0 until 4) {
    io.allocate.allocResp(i) := DontCare
  }
  
  // 处理分配请求
  when (io.allocate.allocReq && canAlloc) {
    val instValid = VecInit(io.allocate.allocEntries.map(_.inst_valid))

    val prefixSum = Wire(Vec(4, UInt(3.W)))
    prefixSum(0) := 0.U
    for (j <- 1 until 4) {
      prefixSum(j) := prefixSum(j-1) + instValid(j-1).asUInt
    }

    for (j <- 0 until 4) {
      when (instValid(j) && (prefixSum(j) < io.allocate.allocCount)) {
        val allocIdx = ((tail + prefixSum(j)) % RobConfig.ROB_ENTRY_NUM.U)(5, 0)
        io.allocate.allocResp(j) := allocIdx
        robEntries(allocIdx) := io.allocate.allocEntries(j)
        robEntries(allocIdx).valid := true.B
        robEntries(allocIdx).finished := false.B
        robEntries(allocIdx).use_preg := io.allocate.allocEntries(j).use_preg
      }
    }
    tail := (tail + io.allocate.allocCount) % RobConfig.ROB_ENTRY_NUM.U
  }
  
  // 指令完成逻辑
  for (i <- 0 until 5) {
    when (io.writeback(i).valid) {
      val idx = io.writeback(i).bits.robIdx
      robEntries(idx).finished     := true.B
      robEntries(idx).exception    := io.writeback(i).bits.exceptionVec.orR
      robEntries(idx).exceptionVec := io.writeback(i).bits.exceptionVec
      robEntries(idx).eret         := robEntries(idx).instr === "b00000110010010000011100000000000".U  // FIXME: hardcode
      robEntries(idx).brMispredict := io.writeback(i).bits.brMispredict
      robEntries(idx).brTarget     := io.writeback(i).bits.brTarget
      robEntries(idx).result       := io.writeback(i).bits.writeData
      robEntries(idx).csrNewData   := io.writeback(i).bits.csrNewData
      val wbRfWen = robEntries(idx).rfWen || io.writeback(i).bits.exceptionVec.orR  // 如果有异常则不写寄存器(0有效)
      robEntries(idx).rfWen        := wbRfWen
      // for load/store difftest
      robEntries(idx).fuType       := io.writeback(i).bits.fuType
      robEntries(idx).paddr        := io.writeback(i).bits.paddr
      robEntries(idx).wdata        := io.writeback(i).bits.wdata
      robEntries(idx).optype       := io.writeback(i).bits.optype
      robEntries(idx).timer64      := io.writeback(i).bits.timer64
    }
  }
  
  // 判断是否可以提交
  val canCommit = Wire(Vec(4, Bool()))
  val hasCsrRW = Wire(Vec(4, Bool()))
  val hasBrMispred = Wire(Vec(4, Bool()))
  val hasException = Wire(Vec(4, Bool()))
  val hasStore = Wire(Vec(4, Bool()))

  for (i <- 0 until 4) {
    val commitIdx = ((head + i.U) % RobConfig.ROB_ENTRY_NUM.U)(5, 0)
    if (i == 0) {
      val preHasCommit = !robEntries(commitIdx - 1.U).valid
      canCommit(i) := robEntries(commitIdx).valid && (robEntries(commitIdx).finished) &&
                      preHasCommit
    } else {
      canCommit(i) := robEntries(commitIdx).valid && (robEntries(commitIdx).finished) &&
                      canCommit(i-1) && !robEntries(commitIdx - 1.U).isStore
    }
    hasCsrRW(i) := robEntries(commitIdx).valid && robEntries(commitIdx).inst_valid &&
                    canCommit(i) && robEntries(commitIdx).csrOp =/= CSROp.nop
    hasException(i) := robEntries(commitIdx).valid && robEntries(commitIdx).inst_valid && 
                       canCommit(i) && (robEntries(commitIdx).exception || robEntries(commitIdx).eret)
    hasBrMispred(i) := canCommit(i) && robEntries(commitIdx).inst_valid && robEntries(commitIdx).brMispredict && !hasException(i)
    hasStore(i) := robEntries(commitIdx).inst_valid && robEntries(commitIdx).isStore &&
                    !hasException(i) && !hasBrMispred(i)
  }

  val store_entry = robEntries(head +& PriorityEncoder(hasStore))

  // 判断在环形缓冲区中idx是否在start之后且在end之前
  def isAfter(idx: UInt, start: UInt, end: UInt): Bool = {
    val isAfterStart = Mux(
      start >= end,
      idx >= start || idx < end,
      idx >= start && idx < end
    )
    isAfterStart
  }

  val csrWrite = hasCsrRW.reduce(_ || _)
  val csrWriteIdx = PriorityEncoder(hasCsrRW)

  val exception = hasException.reduce(_ || _)
  val exceptionIdx = PriorityEncoder(hasException)

  val brMisPred = hasBrMispred.reduce(_ || _)
  val brMisPredIdx = PriorityEncoder(hasBrMispred)

  val minIdx = Wire(UInt(2.W))

  io.brMisPredInfo.brMisPred.valid := brMisPred
  io.brMisPredInfo.brMisPred.bits := robEntries(head + brMisPredIdx).pc
  io.brMisPredInfo.brMisPredTarget := robEntries(head + brMisPredIdx).brTarget
  io.brMisPredInfo.brMisPredChkpt := robEntries(head + brMisPredIdx).checkpoint.id

  // 提交逻辑
  // 生成提交信息
  val shouldCommit = Wire(Vec(4, Bool()))
  val hasCommit = Wire(Vec(4, Bool()))

  for (i <- 0 until 4) {
    val commitIdx = (head +& i.U) % RobConfig.ROB_ENTRY_NUM.U
    val entry = robEntries(commitIdx)
    
    // 在异常或分支预测错误情况下，只提交head---head+x位置的指令

    // 构造候选列表，只加入有效的事件及其下标
    val candidates = Wire(Vec(3, UInt(2.W)))
    val valids = Wire(Vec(3, Bool()))

    candidates(0) := exceptionIdx
    valids(0) := exception
    candidates(1) := brMisPredIdx
    valids(1) := brMisPred
    candidates(2) := csrWriteIdx
    valids(2) := csrWrite

    val temp0 = Mux(valids(0), candidates(0), 3.U)
    val temp1 = Mux(valids(1) && candidates(1) < temp0, candidates(1), temp0)
    val temp2 = Mux(valids(2) && candidates(2) < temp1, candidates(2), temp1)
    minIdx := temp2

    // 是否存在至少一个特殊 commit
    val hasSpecialCommit = valids.reduce(_ || _)

    // 最终是否应该提交
    shouldCommit(i) := Mux(hasSpecialCommit, i.U <= minIdx && canCommit(i), canCommit(i))

    // 物理寄存器回收信息
    io.commit.commit(i).valid := hasCommit(i) && !entry.rfWen && entry.rd =/= 0.U && !entry.exceptionVec(9)  // 9: ALE异常
    // FIXME: Why old_preg?
    // io.commit.commit(i).bits.dest := entry.old_preg
    io.commit.commit(i).bits.pc   := entry.pc
    io.commit.commit(i).bits.dest := entry.rd
    io.commit.commit(i).bits.preg := entry.preg
    io.commit.commit(i).bits.data := Mux(entry.csrOp =/= CSROp.nop && io.plv === 3.U, 0.U, entry.result)
    io.commit.commit(i).bits.inst_valid := entry.inst_valid
    io.commit.commit(i).bits.use_preg := entry.use_preg
    io.commit.commit(i).bits.isBranch := entry.isBranch
    io.commit.commit(i).bits.csr_rstat := (entry.csrOp === CSROp.rd || entry.csrOp === CSROp.xchg || entry.csrOp === CSROp.wr) && entry.csrNum === 5.U
    io.commit.commit(i).bits.csr_data := entry.result
    io.commit.commit(i).bits.excp := entry.exceptionVec.orR
    io.commit.commit(i).bits.timer64 := entry.timer64
    
    // 提交PC信息
    io.commitPC(i).valid := hasCommit(i) && entry.inst_valid
    io.commitPC(i).bits := entry.pc
    
    // 提交指令信息
    io.commitInstr(i).valid := hasCommit(i) && entry.inst_valid
    io.commitInstr(i).bits := entry.instr

    // for csr
    val csr_wen = entry.csrOp === CSROp.wr || entry.csrOp === CSROp.xchg || entry.csrOp === CSROp.ertn
    io.commitCSR(i).valid := hasCommit(i) && entry.inst_valid && csr_wen
    io.commitCSR(i).bits.csr_num := entry.csrNum
    io.commitCSR(i).bits.csr_data := entry.csrNewData

    // for load/store difftest
    io.commitLS(i).valid := hasCommit(i) && entry.inst_valid && entry.fuType === FuType.lsu
    io.commitLS(i).bits.paddr := entry.paddr
    io.commitLS(i).bits.wdata := entry.wdata
    io.commitLS(i).bits.optype := entry.optype
    
    // 清除已提交的条目
    when (hasCommit(i)) {
      robEntries(commitIdx).valid := false.B
    }
  }

  // 提交 Store 状态机
  val st_idle :: st_commit :: st_retire :: Nil = Enum(3)
  
  val st_state = RegInit(st_idle)
  val OutValid = RegInit(false.B)
  val InReady  = RegInit(false.B)
  
  switch (st_state) {
    is (st_idle) {
      when (hasStore.reduce(_ || _) && shouldCommit(PriorityEncoder(hasStore))) {
        st_state := st_commit
        OutValid := true.B
      }
    }
    is (st_commit) {
      when(io.RobLsuOut.ready) {
        OutValid := false.B
        InReady := true.B
        st_state := st_retire
      }
    }
    is (st_retire) {
      when (io.RobLsuIn.valid) {
        InReady := false.B
        st_state := st_idle
      }
    }
  }

  when(io.flush) {
    st_state := st_idle
    OutValid := false.B
    InReady := false.B
  }

  io.RobLsuOut.valid := OutValid
  io.RobLsuIn.ready := InReady

  for (i <- 0 until 4) {
    hasCommit(i) := Mux(hasStore(i), st_state === st_retire && io.RobLsuIn.valid && shouldCommit(i), 
                        shouldCommit(i))
  }

  // 提交异常信息
  val excpOReret = exception && io.commitInstr(exceptionIdx).valid
  val eret = robEntries(head + exceptionIdx).eret && io.commitInstr(exceptionIdx).valid
  io.exceptionInfo.valid := excpOReret && !eret
  io.exceptionInfo.exceptionPC := robEntries(head + exceptionIdx).pc
  io.exceptionInfo.exceptionInst := robEntries(head + exceptionIdx).instr
  io.exceptionInfo.eret := eret
  io.exceptionInfo.exceptionVec := robEntries(head + exceptionIdx).exceptionVec
  io.exceptionInfo.exceptionVAddr := robEntries(head + exceptionIdx).paddr     // FIXME: 物理地址作为异常虚拟地址

  //excp_ine 
    /*
    exceptionVec[0]  int
                [1]  adef
                [2]  tlbr    |inst tlb exceptions
                [3]  pif     |
                [4]  ppi     |
                [5]  syscall
                [6]  brk
                [7]  ine
                [8]  ipe
                [9]  ale
                [10] ertn
                [11] tlbr    |
                [12] pme     |data tlb exceptions
                [13] ppi     |
                [14] pis     |
                [15] pil     |
    */

  val commitNum = PopCount(hasCommit)

  // 更新头指针
  val nextHead = (head + commitNum) % RobConfig.ROB_ENTRY_NUM.U
  when (commitNum > 0.U) {
    head := nextHead
  }

  when (exception || brMisPred || csrWrite) {
    // 回滚ROB尾指针
    val rollbackTail = (head +& minIdx + 1.U) % RobConfig.ROB_ENTRY_NUM.U
    tail := rollbackTail
    // 清除所有在tail之后的条目
    for (i <- 0 until RobConfig.ROB_ENTRY_NUM) {
      val idx = i.U
      when (isAfter(idx, rollbackTail, nextHead)) {
        robEntries(idx).valid := false.B
      }
    }
  }

  io.flush := exception || brMisPred || csrWrite
  val flushEntry = robEntries(head +& minIdx)
  io.newPC := Mux(flushEntry.exception || flushEntry.eret, io.exceptionInfo.exceptionNewPC, 
                  Mux(flushEntry.brMispredict, io.brMisPredInfo.brMisPredTarget, 
                      flushEntry.pc + 4.U))
}