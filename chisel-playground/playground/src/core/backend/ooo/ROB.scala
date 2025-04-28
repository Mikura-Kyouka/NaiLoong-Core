package core

import chisel3._
import chisel3.util._

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
  val old_preg   = UInt(RegConfig.PHYS_REG_BITS.W)
  val rfWen      = Bool()
  val isBranch   = Bool()
  val brMispredict = Bool()
  val brTarget   = UInt(32.W)
  val fuType     = UInt(3.W)
  val result     = UInt(32.W) //FIXME
  val checkpoint = new Bundle {
    val valid = Bool()
    val id    = UInt(RegConfig.CHECKPOINT_DEPTH.W)
  }
  val inst_valid = Bool()
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
}

class rtrBundle extends Bundle {
  val pc   = UInt(32.W)
  val dest = UInt(5.W)
  val preg = UInt(RegConfig.PHYS_REG_BITS.W)
  val data = UInt(32.W)
  val inst_valid = Bool()
}

class BrMisPredInfo extends Bundle {
  val brMisPred = Valid(UInt(32.W))             // 分支预测错误信号
  val brMisPredTarget = UInt(32.W)               // 分支预测错误目标地址
  val brMisPredChkpt = UInt(RegConfig.CHECKPOINT_DEPTH.W) // 分支预测错误检查点ID
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
  
  // 提交接口
  val commit = Output(new RobCommit)                       // 提交信息，用于释放物理寄存器
  val commitPC = Output(Vec(4, Valid(UInt(32.W))))         // 提交的PC
  val commitInstr = Output(Vec(4, Valid(UInt(32.W))))      // 提交的指令

  // 分支预测错误接口
  val brMisPredInfo = Output(new BrMisPredInfo)

  // 异常接口
  val exception = Output(Bool())                           // 异常信号
  val exceptionPC = Output(UInt(32.W))                     // 异常PC
  val exceptionInfo = Output(UInt(16.W))                   // 异常信息
  
  // 调试接口
  // val debug = Output(new Bundle {
  //   val robHead = UInt(RobConfig.ROB_INDEX_WIDTH.W)
  //   val robTail = UInt(RobConfig.ROB_INDEX_WIDTH.W)
  //   val robCount = UInt((RobConfig.ROB_INDEX_WIDTH + 1).W)
  //   val full = Bool()
  //   val empty = Bool()
  // })
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
    val allocIdx = (tail +& i.U) % RobConfig.ROB_ENTRY_NUM.U
    io.allocate.allocResp(i) := allocIdx
  }
  
  // 处理分配请求
  when (io.allocate.allocReq && canAlloc) {
    for (i <- 0 until 4) {
      when (i.U < io.allocate.allocCount) {
        // FIXME: [W004] Dynamic index with width 7 is too wide for Vec of size 64 (expected index width 6).
        val allocIdx = ((tail +& i.U) % RobConfig.ROB_ENTRY_NUM.U)(5, 0)
        robEntries(allocIdx) := io.allocate.allocEntries(i)
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
      robEntries(idx).exception    := io.writeback(i).bits.exception
      robEntries(idx).exceptionVec := io.writeback(i).bits.exceptionVec
      robEntries(idx).brMispredict := io.writeback(i).bits.brMispredict
      robEntries(idx).brTarget     := io.writeback(i).bits.brTarget
      robEntries(idx).result       := io.writeback(i).bits.writeData
    }
  }
  
  // 判断是否可以提交
  val canCommit = Wire(Vec(4, Bool()))
  val hasBrMispred = Wire(Vec(4, Bool()))
  val hasException = Wire(Vec(4, Bool()))

  for (i <- 0 until 4) {
    // FIXME: [W004] Dynamic index with width 7 is too wide for Vec of size 64 (expected index width 6).
    // Don't need +& for round queue?
    val commitIdx = ((head + i.U) % RobConfig.ROB_ENTRY_NUM.U)(5, 0)
    if (i == 0) {
      val preHasCommit = !robEntries(commitIdx - 1.U).valid
      canCommit(i) := robEntries(commitIdx).valid && robEntries(commitIdx).finished &&
                      preHasCommit && !io.exception
    } else {
      canCommit(i) := robEntries(commitIdx).valid && robEntries(commitIdx).finished &&
                      canCommit(i-1) && !io.exception
    }
    hasException(i) := robEntries(commitIdx).valid && robEntries(commitIdx).finished && robEntries(commitIdx).exception
    hasBrMispred(i) := robEntries(commitIdx).valid && robEntries(commitIdx).finished && robEntries(commitIdx).brMispredict && !hasException(i)
  }
  
  // 判断在环形缓冲区中idx是否在start之后且在end之前
  def isAfter(idx: UInt, start: UInt, end: UInt): Bool = {
    val isAfterStart = Mux(
      start <= end,
      idx > start && idx < end,
      idx > start || idx < end
    )
    isAfterStart
  }

  val exception = hasException.reduce(_ || _)
  val exceptionIdx = PriorityEncoder(hasException)

  val brMisPred = hasBrMispred.reduce(_ || _)
  val brMisPredIdx = PriorityEncoder(hasBrMispred)

  io.exception := exception
  io.exceptionPC := robEntries(head + exceptionIdx).pc
  io.exceptionInfo := robEntries(head + exceptionIdx).exceptionVec

  io.brMisPredInfo.brMisPred.valid := brMisPred
  io.brMisPredInfo.brMisPred.bits := robEntries(head + brMisPredIdx).pc
  io.brMisPredInfo.brMisPredTarget := robEntries(head + brMisPredIdx).brTarget
  io.brMisPredInfo.brMisPredChkpt := robEntries(head + brMisPredIdx).checkpoint.id

  // 分支预测错误时，需要将tail回滚到head+x的位置
  when (brMisPred) {
    // 回滚ROB尾指针
    // printf("ROB: Rollback tail from %d to %d\n", tail, head +& brMisPredIdx +& 1.U)
    tail := (head +& brMisPredIdx +& 1.U) % RobConfig.ROB_ENTRY_NUM.U
    // 清除所有在tail之后的条目
    for (i <- 0 until RobConfig.ROB_ENTRY_NUM) {
      val idx = i.U
      when (isAfter(idx, head, tail)) {
        // printf("ROB: Clear entry %d\n", idx)
        robEntries(idx).valid := false.B
      }
    }
  }

  // 提交逻辑

  // 生成提交信息
  for (i <- 0 until 4) {
    /* 
    +& 运算符与普通的 + 运算符不同。
    它执行加法运算时会扩展结果的位宽，以便包含加法中产生的进位。
    例如，当对两个 UInt 进行加法时，+& 会保证结果拥有足够的位数来表示整个和，即使会有溢出（carry）产生。
    这在硬件设计中非常重要，因为它可以防止因位宽不足而丢失溢出信息。
     */
    val commitIdx = (head +& i.U) % RobConfig.ROB_ENTRY_NUM.U
    val entry = robEntries(commitIdx)
    
    // 在异常或分支预测错误情况下，只提交head位置的指令
    val shouldCommit = Mux (
      exception || brMisPred,
      i.U <= brMisPredIdx && canCommit(brMisPredIdx),
      canCommit(i)
    )
    
    // 物理寄存器回收信息
    io.commit.commit(i).valid := shouldCommit && !entry.rfWen && entry.rd =/= 0.U
    // FIXME: Why old_preg?
    // io.commit.commit(i).bits.dest := entry.old_preg
    io.commit.commit(i).bits.pc   := entry.pc
    io.commit.commit(i).bits.dest := entry.rd
    io.commit.commit(i).bits.preg := entry.preg
    io.commit.commit(i).bits.data := entry.result
    io.commit.commit(i).bits.inst_valid := entry.inst_valid
    
    // 提交PC信息
    io.commitPC(i).valid := shouldCommit
    io.commitPC(i).bits := entry.pc
    
    // 提交指令信息
    io.commitInstr(i).valid := shouldCommit
    io.commitInstr(i).bits := entry.instr
    
    // 清除已提交的条目
    when (shouldCommit) {
      robEntries(commitIdx).valid := false.B
    }
  }

  val commitNum = PopCount(io.commitInstr.map(_.valid))

  // 更新头指针
  when (commitNum > 0.U) {
    head := (head + commitNum) % RobConfig.ROB_ENTRY_NUM.U
  }
}