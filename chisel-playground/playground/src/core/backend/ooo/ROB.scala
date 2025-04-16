package core

import chisel3._
import chisel3.util._

object RobConfig {
  val ROB_ENTRY_NUM = 64
  val ROB_INDEX_WIDTH = log2Ceil(ROB_ENTRY_NUM)
}

class RobEntry extends Bundle {
  val valid      = Bool()
  val finished   = Bool()
  val pc         = UInt(64.W)
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
  val brTarget   = UInt(64.W)
  val fuType     = UInt(3.W)
  val checkpoint = new Bundle {
    val valid = Bool()
    val id    = UInt(RegConfig.CHECKPOINT_DEPTH.W)
  }
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
  val robIdx     = UInt(RobConfig.ROB_INDEX_WIDTH.W)
  val exception  = Bool()
  val exceptionVec = UInt(16.W)
  val brMispredict = Bool()
  val brTarget   = UInt(64.W)
  val writeData = UInt(32.W)
}

// 提交接口
class RobCommit extends Bundle {
  val commit = Vec(4, Valid(UInt(RegConfig.PHYS_REG_BITS.W)))
}

class RobIO extends Bundle {
  // 分配接口
  val allocate = Flipped(new RobAllocateIO)
  
  // 指令完成接口
  val writeback = Vec(5, Flipped(Valid(new RobWritebackInfo)))
  
  // 提交接口
  val commit = Output(new RobCommit)                       // 提交信息，用于释放物理寄存器
  val commitPC = Output(Vec(4, Valid(UInt(64.W))))         // 提交的PC
  val commitInstr = Output(Vec(4, Valid(UInt(32.W))))      // 提交的指令
  
  // 分支预测错误接口
  val brMisPred = Output(Valid(UInt(64.W)))                // 分支预测错误信号
  val brMisPredTarget = Output(UInt(64.W))                 // 分支预测错误目标地址
  val brMisPredChkpt = Output(UInt(RegConfig.CHECKPOINT_DEPTH.W)) // 分支预测错误检查点ID
  
  // 异常接口
  val exception = Output(Bool())                           // 异常信号
  val exceptionPC = Output(UInt(64.W))                     // 异常PC
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
  for (i <- 0 until 4) {
    when (io.writeback(i).valid) {
      val idx = io.writeback(i).bits.robIdx
      robEntries(idx).finished := true.B
      robEntries(idx).exception := io.writeback(i).bits.exception
      robEntries(idx).exceptionVec := io.writeback(i).bits.exceptionVec
      robEntries(idx).brMispredict := io.writeback(i).bits.brMispredict
      robEntries(idx).brTarget := io.writeback(i).bits.brTarget
    }
  }
  
  // 判断是否可以提交
  val canCommit = Wire(Vec(4, Bool()))
  for (i <- 0 until 4) {
    // FIXME: [W004] Dynamic index with width 7 is too wide for Vec of size 64 (expected index width 6).
    val commitIdx = ((head +& i.U) % RobConfig.ROB_ENTRY_NUM.U)(5, 0)
    if (i == 0) {
      canCommit(i) := robEntries(commitIdx).valid && robEntries(commitIdx).finished &&
                      !io.brMisPred.valid && !io.exception
    } else {
      canCommit(i) := robEntries(commitIdx).valid && robEntries(commitIdx).finished &&
                      canCommit(i-1) &&
                      !io.brMisPred.valid && !io.exception
    }
  }
  
  // 检查异常和分支预测错误
  val hasException = robEntries(head).valid && robEntries(head).finished && robEntries(head).exception
  val hasBrMispred = robEntries(head).valid && robEntries(head).finished && robEntries(head).brMispredict && !hasException
  
  io.exception := hasException
  io.exceptionPC := robEntries(head).pc
  io.exceptionInfo := robEntries(head).exceptionVec
  
  io.brMisPred.valid := hasBrMispred
  io.brMisPred.bits := robEntries(head).pc
  io.brMisPredTarget := robEntries(head).brTarget
  io.brMisPredChkpt := robEntries(head).checkpoint.id
  
  // 提交逻辑
  val commitNum = PopCount(canCommit)
  
  // 生成提交信息
  for (i <- 0 until 4) {
    val commitIdx = (head +& i.U) % RobConfig.ROB_ENTRY_NUM.U
    val entry = robEntries(commitIdx)
    
    // 物理寄存器回收信息
    io.commit.commit(i).valid := canCommit(i) && !entry.rfWen && entry.rd =/= 0.U
    io.commit.commit(i).bits := entry.old_preg
    
    // 提交PC信息
    io.commitPC(i).valid := canCommit(i)
    io.commitPC(i).bits := entry.pc
    
    // 提交指令信息
    io.commitInstr(i).valid := canCommit(i)
    io.commitInstr(i).bits := entry.instr
    
    // 清除已提交的条目
    when (canCommit(i)) {
      robEntries(commitIdx).valid := false.B
    }
  }
  
  // 更新头指针
  when (commitNum > 0.U) {
    head := (head + commitNum) % RobConfig.ROB_ENTRY_NUM.U
  }
  
  // 调试接口
  // io.debug.robHead := head
  // io.debug.robTail := tail
  // io.debug.robCount := count
  // io.debug.full := full
  // io.debug.empty := empty
}