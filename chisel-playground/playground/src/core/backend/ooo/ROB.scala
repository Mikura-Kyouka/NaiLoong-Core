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
  val ROB_CMT_NUM = 2
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
  val brTaken    = Bool()
  val brTarget   = UInt(32.W)
  val isCall     = Bool()
  val isReturn   = Bool()
  val fuType     = UInt(3.W)
  val result     = UInt(32.W) //FIXME
  val checkpoint = new Bundle {
    val valid = Bool()
    val id    = UInt(RegConfig.CHECKPOINT_DEPTH.W)
  }
  val inst_valid = Bool()
  val csrOp     = UInt(4.W)
  val csrNum    = UInt(14.W)
  val csrNewData = UInt(32.W)
  val eret = Bool() 
  val tlbInfo = new TlbInstBundle
  val cacopOp = UInt(2.W)
  val cType = UInt(2.W)
  val failsc = Bool()

  // for load/store difftest
  val paddr      = UInt(32.W)
  val vaddr      = UInt(32.W)
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
  val redirect     = new RedirectIO
  val writeData    = UInt(32.W)
  val csrNewData   = UInt(32.W)

  // for load/store difftest
  val paddr       = UInt(32.W)
  val vaddr       = UInt(32.W)
  val wdata       = UInt(32.W)
  val fuType      = UInt(3.W)
  val optype      = UInt(7.W)
  val timer64     = UInt(64.W)
  val tlbInfo     = new TlbInstBundle
  val failsc      = Bool() // 是否发生了失败的sc指令
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
  val actuallyTaken = Bool()
  val brMisPredTarget = UInt(32.W)               // 分支预测错误目标地址
  // val brMisPredChkpt = UInt(RegConfig.CHECKPOINT_DEPTH.W) // 分支预测错误检查点ID
  val brMisPredPC = UInt(32.W)
  val isCall = Bool()
  val isReturn = Bool()
}

class LSCommitInfo extends Bundle {
  val paddr = UInt(32.W) // 物理地址
  val vaddr = UInt(32.W) // 虚拟地址
  val wdata = UInt(32.W) // 写入数据
  val optype = UInt(7.W) // 操作类型
}

// 提交接口
class RobCommit extends Bundle {
  val commit = Vec(RobConfig.ROB_CMT_NUM, Valid(new rtrBundle))
}

class RobIO extends Bundle {
  // 分配接口
  val allocate = Flipped(new RobAllocateIO)
  
  // 指令完成接口
  val writeback = Vec(5, Flipped(Valid(new RobWritebackInfo)))

  // ROB 和 LSU 关于 Store 指令的交互
  val scommit = Output(Bool())
  
  // 提交接口
  val commit = Output(new RobCommit)                       // 提交信息，用于释放物理寄存器
  val commitPC = Output(Vec(RobConfig.ROB_CMT_NUM, Valid(UInt(32.W))))         // 提交的PC
  val commitInstr = Output(Vec(RobConfig.ROB_CMT_NUM, Valid(UInt(32.W))))      // 提交的指令
  // for load/store difftest
  val commitLS = Output(Vec(RobConfig.ROB_CMT_NUM, Valid(new LSCommitInfo)))   // 提交的store信息
  val commitCSR = Vec(RobConfig.ROB_CMT_NUM, Valid(new csr_write_bundle))

  // 分支预测错误接口
  val brMisPredInfo = Output(new BrMisPredInfo)

  val brTrainInfo = Output(new BrMisPredInfo)

  // 异常接口
  val exceptionInfo = Flipped(new csr_excp_bundle)

  // TLB 操作接口
  val tlbInfo = Output(new TlbInstBundle)

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
      robEntries(idx).exceptionVec := Cat(io.writeback(i).bits.exceptionVec(15, 9),
                                          ((robEntries(idx).csrOp === CSROp.rd ||
                                          robEntries(idx).csrOp === CSROp.xchg ||
                                          robEntries(idx).csrOp === CSROp.wr) && io.plv === 3.U),
                                          io.writeback(i).bits.exceptionVec(7, 0))
      robEntries(idx).eret         := io.writeback(i).bits.exceptionVec(10)
      robEntries(idx).brMispredict := Mux(io.writeback(i).bits.redirect.actuallyTaken =/= io.writeback(i).bits.redirect.predictTaken, 
                                          true.B, 
                                          Mux(io.writeback(i).bits.redirect.actuallyTaken, 
                                              io.writeback(i).bits.redirect.actuallyTarget =/= io.writeback(i).bits.redirect.predictTarget,
                                              false.B))
      robEntries(idx).brTarget     := Mux(io.writeback(i).bits.redirect.actuallyTaken, 
                                          io.writeback(i).bits.redirect.actuallyTarget, 
                                          io.writeback(i).bits.pc + 4.U)
      robEntries(idx).brTaken      := io.writeback(i).bits.redirect.actuallyTaken
      robEntries(idx).result       := io.writeback(i).bits.writeData
      robEntries(idx).csrNewData   := io.writeback(i).bits.csrNewData
      val wbRfWen = robEntries(idx).rfWen || io.writeback(i).bits.exceptionVec.orR  // 如果有异常则不写寄存器(0有效)
      robEntries(idx).rfWen        := wbRfWen
      // for load/store difftest
      robEntries(idx).paddr        := io.writeback(i).bits.paddr
      robEntries(idx).vaddr        := io.writeback(i).bits.vaddr
      robEntries(idx).wdata        := io.writeback(i).bits.wdata
      robEntries(idx).timer64      := io.writeback(i).bits.timer64
      robEntries(idx).tlbInfo      := io.writeback(i).bits.tlbInfo
      robEntries(idx).failsc       := io.writeback(i).bits.failsc
    }
  }
  
  // 判断是否可以提交
  val canCommit = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val hasCsrRW = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val hasBrMispred = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val hasBr = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val hasException = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val hasStore = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val hasTlb = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  for (i <- 0 until RobConfig.ROB_CMT_NUM) {
    val commitIdx = ((head + i.U) % RobConfig.ROB_ENTRY_NUM.U)(5, 0)
    if (i == 0) {
      val preshouldCommit = !robEntries(commitIdx - 1.U).valid
      canCommit(i) := robEntries(commitIdx).valid && (robEntries(commitIdx).finished) &&
                      preshouldCommit
    } else {
      canCommit(i) := robEntries(commitIdx).valid && (robEntries(commitIdx).finished) &&
                      canCommit(i-1) && !robEntries(commitIdx - 1.U).isStore
    }
    hasCsrRW(i) := robEntries(commitIdx).valid && robEntries(commitIdx).inst_valid &&
                    canCommit(i) && robEntries(commitIdx).csrOp =/= CSROp.nop
    hasException(i) := robEntries(commitIdx).valid && robEntries(commitIdx).inst_valid && 
                       canCommit(i) && (robEntries(commitIdx).exception || robEntries(commitIdx).eret)
    hasBrMispred(i) := canCommit(i) && robEntries(commitIdx).inst_valid && robEntries(commitIdx).brMispredict && !hasException(i)
    hasBr(i) := canCommit(i) && robEntries(commitIdx).inst_valid && 
                robEntries(commitIdx).fuType === FuType.bru && ALUOpType.isBru(robEntries(commitIdx).optype) &&
                !hasException(i)
    hasStore(i) := robEntries(commitIdx).inst_valid && robEntries(commitIdx).isStore &&
                    !hasException(i)
    hasTlb(i) := robEntries(commitIdx).valid && robEntries(commitIdx).inst_valid &&      // FIXME: cacop as tlb operation
                  (robEntries(commitIdx).tlbInfo.inst_type =/= TlbOp.nop ||
                   (robEntries(commitIdx).cacopOp =/= CACOPOp.nop) && robEntries(commitIdx).cType === CACOPType.i) &&
                  canCommit(i) && !hasException(i)
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

  // 提交逻辑
  // 生成提交信息

  val shouldCommit = Wire(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val csrWrite = hasCsrRW.reduce(_ || _)
  val csrWriteIdx = PriorityEncoder(hasCsrRW)

  val exception = hasException.reduce(_ || _)
  val exceptionIdx = PriorityEncoder(hasException)

  val tlbOperation = hasTlb.reduce(_ || _)
  val tlbIdx = PriorityEncoder(hasTlb)

  val brMisPredTemp = hasBrMispred.reduce(_ || _)
  val brMisPredIdx = PriorityEncoder(hasBrMispred)
  val brMisPred = brMisPredTemp

  val minIdx = Wire(UInt(2.W))

  io.brMisPredInfo.brMisPred.valid := brMisPred
  io.brMisPredInfo.brMisPred.bits := robEntries(head + brMisPredIdx).pc
  io.brMisPredInfo.brMisPredTarget := robEntries(head + brMisPredIdx).brTarget
  // io.brMisPredInfo.brMisPredChkpt := robEntries(head + brMisPredIdx).checkpoint.id
  io.brMisPredInfo.brMisPredPC := robEntries(head + brMisPredIdx).pc
  io.brMisPredInfo.actuallyTaken := robEntries(head + brMisPredIdx).brTaken
  io.brMisPredInfo.isCall := robEntries(head + brMisPredIdx).isCall
  io.brMisPredInfo.isReturn := robEntries(head + brMisPredIdx).isReturn


  val br = hasBr.reduce(_ || _)
  val brIdx = PriorityEncoder(hasBr)
  io.brTrainInfo.brMisPred.valid := br
  io.brTrainInfo.brMisPred.bits := robEntries(head + brIdx).pc
  io.brTrainInfo.brMisPredTarget := robEntries(head + brIdx).brTarget
  io.brTrainInfo.brMisPredPC := robEntries(head + brIdx).pc
  io.brTrainInfo.actuallyTaken := robEntries(head + brIdx).brTaken
  io.brTrainInfo.isCall := robEntries(head + brIdx).isCall
  io.brTrainInfo.isReturn := robEntries(head + brIdx).isReturn

  for (i <- 0 until RobConfig.ROB_CMT_NUM) {
    val commitIdx = (head +& i.U) % RobConfig.ROB_ENTRY_NUM.U
    val entry = robEntries(commitIdx)
    
    // 在异常或分支预测错误情况下，只提交head---head+x位置的指令

    // 构造候选列表，只加入有效的事件及其下标
    val candidates = Wire(Vec(4, UInt(2.W)))
    val valids = Wire(Vec(4, Bool()))

    candidates(0) := exceptionIdx
    valids(0) := exception
    candidates(1) := brMisPredIdx
    valids(1) := brMisPred
    candidates(2) := csrWriteIdx
    valids(2) := csrWrite
    candidates(3) := tlbIdx
    valids(3) := tlbOperation

    val temp0 = Mux(valids(0), candidates(0), 3.U)
    val temp1 = Mux(valids(1) && candidates(1) < temp0, candidates(1), temp0)
    val temp2 = Mux(valids(2) && candidates(2) < temp1, candidates(2), temp1)
    val temp3 = Mux(valids(3) && candidates(3) < temp2, candidates(3), temp2)
    minIdx := temp3

    // 是否存在至少一个特殊 commit
    val hasSpecialCommit = valids.reduce(_ || _)

    // 最终是否应该提交
    shouldCommit(i) := Mux(hasSpecialCommit, i.U <= minIdx && canCommit(i), canCommit(i))

    // 物理寄存器回收信息
    io.commit.commit(i).valid := shouldCommit(i) && !entry.rfWen && entry.rd =/= 0.U && !entry.exceptionVec(9)  // 9: ALE异常
    // FIXME: Why old_preg?
    // io.commit.commit(i).bits.dest := entry.old_preg
    io.commit.commit(i).bits.pc   := entry.pc
    io.commit.commit(i).bits.dest := entry.rd
    io.commit.commit(i).bits.preg := entry.preg
    io.commit.commit(i).bits.data := entry.result
    io.commit.commit(i).bits.inst_valid := entry.inst_valid
    io.commit.commit(i).bits.use_preg := entry.use_preg
    io.commit.commit(i).bits.isBranch := entry.isBranch
    io.commit.commit(i).bits.csr_rstat := (entry.csrOp === CSROp.rd || entry.csrOp === CSROp.xchg || entry.csrOp === CSROp.wr) && entry.csrNum === 5.U
    io.commit.commit(i).bits.csr_data := entry.result
    io.commit.commit(i).bits.excp := entry.exceptionVec.orR && !entry.eret
    io.commit.commit(i).bits.timer64 := entry.timer64
    
    // 提交PC信息
    io.commitPC(i).valid := shouldCommit(i) && entry.inst_valid
    io.commitPC(i).bits := entry.pc
    
    // 提交指令信息
    io.commitInstr(i).valid := shouldCommit(i) && entry.inst_valid
    io.commitInstr(i).bits := entry.instr

    // for csr
    val csr_wen = entry.csrOp === CSROp.wr || entry.csrOp === CSROp.xchg || entry.csrOp === CSROp.ertn ||
                  entry.csrOp === CSROp.ll || entry.csrOp === CSROp.sc || entry.csrOp === CSROp.idle
    io.commitCSR(i).valid := shouldCommit(i) && entry.inst_valid && csr_wen && !hasException(i)
    io.commitCSR(i).bits.csr_num := entry.csrNum
    io.commitCSR(i).bits.csr_data := entry.csrNewData
    io.commitCSR(i).bits.ll := entry.csrOp === CSROp.ll
    io.commitCSR(i).bits.sc := entry.csrOp === CSROp.sc
    io.commitCSR(i).bits.lladdr := entry.vaddr // ll指令的地址
    io.commitCSR(i).bits.idle := entry.csrOp === CSROp.idle // 是否是idle指令

    // for load/store difftest
    io.commitLS(i).valid := shouldCommit(i) && entry.inst_valid && entry.fuType === FuType.lsu && !entry.failsc
    io.commitLS(i).bits.paddr := entry.paddr
    io.commitLS(i).bits.vaddr := entry.vaddr
    io.commitLS(i).bits.wdata := entry.wdata
    io.commitLS(i).bits.optype := entry.optype

    // 清除已提交的条目
    when (shouldCommit(i)) {
      robEntries(commitIdx).valid := false.B
    }
  }

  // 提交异常信息
  val excpOReret = exception && io.commitInstr(exceptionIdx).valid
  val eret = robEntries(head + exceptionIdx).eret && io.commitInstr(exceptionIdx).valid
  io.exceptionInfo.valid := excpOReret && !eret
  io.exceptionInfo.exceptionPC := robEntries(head + exceptionIdx).pc
  io.exceptionInfo.exceptionInst := robEntries(head + exceptionIdx).instr
  io.exceptionInfo.eret := eret
  io.exceptionInfo.exceptionVec := robEntries(head + exceptionIdx).exceptionVec
  io.exceptionInfo.exceptionVAddr := robEntries(head + exceptionIdx).vaddr
  io.exceptionInfo.idle := robEntries(head + exceptionIdx).fuType === FuType.bru &&
                           robEntries(head + exceptionIdx).optype === ALUOpType.idle

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

  io.tlbInfo := robEntries(head + tlbIdx).tlbInfo
  io.tlbInfo.en := tlbOperation && io.commitInstr(tlbIdx).valid && io.tlbInfo.inst_type =/= TlbOp.nop

  io.scommit := hasStore.reduce(_ || _) && shouldCommit(PriorityEncoder(hasStore)) && 
                !robEntries(head + PriorityEncoder(hasStore)).failsc

  val commitNum = PopCount(shouldCommit)

  // 更新头指针
  val nextHead = (head + commitNum) % RobConfig.ROB_ENTRY_NUM.U
  when (commitNum > 0.U) {
    head := nextHead
  }

  when ((exception || brMisPred || csrWrite || tlbOperation) && shouldCommit.reduce(_ || _)) {
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

  io.flush := (exception || brMisPred || csrWrite || tlbOperation) && shouldCommit.reduce(_ || _)
  val flushEntry = robEntries(head +& minIdx)
  io.newPC := Mux(flushEntry.exception || flushEntry.eret, io.exceptionInfo.exceptionNewPC, 
                  Mux(flushEntry.brMispredict, io.brMisPredInfo.brMisPredTarget, 
                      flushEntry.pc + 4.U))

  if(GenCtrl.USE_COUNT) {
    // Initialize to 1 to avoid division by zero
    val brInstCount = RegInit(1.U(32.W))
    val brMisPredCount = RegInit(1.U(32.W))
    val foo = RegInit(0.U(32.W))

    when(io.commitInstr.map(_.valid).reduce(_ || _)) {
      brMisPredCount := brMisPredCount + Mux(io.brMisPredInfo.brMisPred.valid, 1.U, 0.U)

      brInstCount := brInstCount + io.commitInstr.zip(hasBr).map { case (instr, br) => 
        Mux(instr.valid && br, 1.U, 0.U) 
      }.reduce(_ +& _)
      
      foo := (foo + 1.U) % 500.U
      when(foo === 0.U) {
        printf("[ROB] Average BR misprediction rate: %d/%d = %d%%\n", brMisPredCount, brInstCount, 
             (brMisPredCount * 100.U) / brInstCount)
      }
    }
  }
}