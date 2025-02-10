package core

import chisel3._
import chisel3.util._

object RegConfig {
  val ARCH_REG_NUM    = 32
  val PHYS_REG_NUM    = 128
  val PHYS_REG_BITS   = log2Ceil(PHYS_REG_NUM)
  val FREE_LIST_SIZE  = PHYS_REG_NUM - ARCH_REG_NUM
  val FREE_LIST_BITS  = log2Ceil(FREE_LIST_SIZE + 1)
}

class DispatchEntry extends Bundle {
  val pc         = UInt(32.W)
  val areg       = UInt(5.W)
  val preg       = UInt(RegConfig.PHYS_REG_BITS.W)
  val opreg      = UInt(RegConfig.PHYS_REG_BITS.W)
  val instType   = UInt(3.W)
  val checkpoint = UInt(6.W)
}

class RenameCheckpoint extends Bundle {
  val rmt       = Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W))
  val freeHead  = UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)
  val freeTail  = UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)
  val freeCount = UInt(RegConfig.FREE_LIST_BITS.W)
}

class RegRenamingIO extends Bundle {
  val disp_valid  = Input(Vec(4, Bool()))
  val disp_info   = Input(Vec(4, new DispatchEntry))
  val ren_disp_info = Output(Vec(4, new DispatchEntry))
  val ren_alloc_ready = Output(Bool())

  val commit_free_valid = Input(Vec(4, Bool()))
  val commit_free_preg  = Input(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))

  val recover     = Input(Bool())
  val recover_chk = Input(UInt(6.W))
  val recover_done = Output(Bool())

  val free_list_count = Output(UInt(RegConfig.FREE_LIST_BITS.W))
}

class RegRenaming extends Module {
  val io = IO(new RegRenamingIO)

  val rmt = RegInit(VecInit(Seq.tabulate(RegConfig.ARCH_REG_NUM){ i => i.U(RegConfig.PHYS_REG_BITS.W) }))

  val freeList = RegInit(VecInit((RegConfig.ARCH_REG_NUM until RegConfig.PHYS_REG_NUM)
    .map(i => i.U(RegConfig.PHYS_REG_BITS.W))))
  val freeHead = RegInit(0.U(log2Ceil(RegConfig.FREE_LIST_SIZE).W))
  val freeTail = RegInit(0.U(log2Ceil(RegConfig.FREE_LIST_SIZE).W))
  val freeCount = RegInit(RegConfig.FREE_LIST_SIZE.U(RegConfig.FREE_LIST_BITS.W))

  val numCheckpoints = 64
  val chkRmt       = Reg(Vec(numCheckpoints, Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W))))
  val chkFreeHead  = Reg(Vec(numCheckpoints, UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)))
  val chkFreeTail  = Reg(Vec(numCheckpoints, UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)))
  val chkFreeCount = Reg(Vec(numCheckpoints, UInt(RegConfig.FREE_LIST_BITS.W)))
  val chkValid     = RegInit(VecInit(Seq.fill(numCheckpoints)(false.B)))
  
  val nextChkId = RegInit(0.U(6.W))

  io.recover_done := false.B

  val allocCount = PopCount(io.disp_valid)
  io.ren_alloc_ready := freeCount >= allocCount

  def wrapIndex(idx: UInt): UInt = {
    Mux(idx >= RegConfig.FREE_LIST_SIZE.U, idx - RegConfig.FREE_LIST_SIZE.U, idx)
  }

  val branchValid = Wire(Vec(4, Bool()))
  for(i <- 0 until 4) {
    branchValid(i) := io.disp_valid(i) && (io.disp_info(i).instType === InstType.branch)
  }
  val totalBranches = branchValid.map(_.asUInt).reduce(_ +& _)

  val rmt_stage = Wire(Vec(5, Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W))))
  rmt_stage(0) := rmt

  val newPregs = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
  val opRegs   = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
  val chkFields= Wire(Vec(4, UInt(6.W)))
  for(i <- 0 until 4) {
    newPregs(i) := 0.U
    opRegs(i)   := 0.U
    chkFields(i):= 0.U
  }

  for(i <- 0 until 4) {
    when(io.disp_valid(i)) {
      val arch = io.disp_info(i).areg
      val op_reg = Wire(UInt(RegConfig.PHYS_REG_BITS.W))
      op_reg := rmt_stage(i)(arch)
      for(j <- (0 until i).reverse) {
        when(io.disp_valid(j) && (io.disp_info(j).areg === arch)) {
          op_reg := newPregs(j)
        }
      }
      opRegs(i) := op_reg

      val allocIndex = wrapIndex(freeHead + i.U)
      newPregs(i) := freeList(allocIndex)

      val next_rmt = Wire(Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W)))
      for(k <- 0 until RegConfig.ARCH_REG_NUM) {
        next_rmt(k) := rmt_stage(i)(k)
      }
      next_rmt(arch) := newPregs(i)
      rmt_stage(i+1) := next_rmt

      when(branchValid(i)) {
        val branchCountBefore = if(i == 0) 0.U else (0 until i).map(j => branchValid(j).asUInt).reduce(_ +& _)
        val chkIdForThis = nextChkId + branchCountBefore
        chkFields(i) := chkIdForThis
        for(k <- 0 until RegConfig.ARCH_REG_NUM) {
          chkRmt(chkIdForThis)(k) := rmt_stage(i+1)(k)
        }
        val newFreeHead = wrapIndex(freeHead + i.U)
        chkFreeHead(chkIdForThis) := newFreeHead
        chkFreeTail(chkIdForThis) := freeTail
        chkFreeCount(chkIdForThis) := freeCount - i.U
        chkValid(chkIdForThis) := true.B
      } .otherwise {
        chkFields(i) := 0.U
      }
    } .otherwise {
      rmt_stage(i+1) := rmt_stage(i)
      opRegs(i)   := 0.U
      newPregs(i) := 0.U
      chkFields(i):= 0.U
    }
  }

  val renEntries = Wire(Vec(4, new DispatchEntry))
  for(i <- 0 until 4) {
    renEntries(i).pc         := io.disp_info(i).pc
    renEntries(i).areg       := io.disp_info(i).areg
    renEntries(i).preg       := newPregs(i)
    renEntries(i).opreg      := opRegs(i)
    renEntries(i).instType   := io.disp_info(i).instType
    renEntries(i).checkpoint := chkFields(i)
  }
  io.ren_disp_info := renEntries

  when(io.ren_alloc_ready && !io.recover) {
    rmt := rmt_stage(4)
    val new_freeHead = wrapIndex(freeHead + allocCount)
    freeHead := new_freeHead
    freeCount := freeCount - allocCount
  }

  val commitFreeCount = PopCount(io.commit_free_valid)
  for(i <- 0 until 4) {
    when(io.commit_free_valid(i)) {
      val enqueueIndex = wrapIndex(freeTail + i.U) // 顺序写入
      freeList(enqueueIndex) := io.commit_free_preg(i)
    }
  }
  freeTail := wrapIndex(freeTail + commitFreeCount)
  freeCount := freeCount + commitFreeCount

  when(io.recover) {
    when(chkValid(io.recover_chk)) {
      for(i <- 0 until RegConfig.ARCH_REG_NUM) {
        rmt(i) := chkRmt(io.recover_chk)(i)
      }
      freeHead := chkFreeHead(io.recover_chk)
      freeTail := chkFreeTail(io.recover_chk)
      freeCount := chkFreeCount(io.recover_chk)
      chkValid(io.recover_chk) := false.B
      io.recover_done := true.B
    } .otherwise {
      io.recover_done := false.B
    }
  }

  io.free_list_count := freeCount

  when(io.ren_alloc_ready && !io.recover) {
    nextChkId := nextChkId + totalBranches
  }
}
