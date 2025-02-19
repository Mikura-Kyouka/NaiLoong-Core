package core

import chisel3._
import chisel3.util._

object RegConfig {
  val ARCH_REG_NUM = 32
  val PHYS_REG_NUM = 128
  val PHYS_REG_BITS = 7 
  val CHECKPOINT_DEPTH = 8
}

class RenameInput extends Bundle {
  val rj     = UInt(5.W)
  val rk     = UInt(5.W)
  val rd     = UInt(5.W)
}

class RenameOutput extends Bundle {
  val prj    = UInt(RegConfig.PHYS_REG_BITS.W)
  val prk    = UInt(RegConfig.PHYS_REG_BITS.W)
  val preg   = UInt(RegConfig.PHYS_REG_BITS.W)
  val old_preg = UInt(RegConfig.PHYS_REG_BITS.W)
}

class RegRenaming extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(4, new RenameInput))
    val out = Output(Vec(4, new RenameOutput))
    val checkpoint_save = Input(Bool())
    val checkpoint_id = Input(UInt(RegConfig.CHECKPOINT_DEPTH.W))
    val recover = Input(Bool())
    val recover_chk = Input(UInt(RegConfig.CHECKPOINT_DEPTH.W))
    val recover_done = Output(Bool())
    val alloc_ready = Output(Bool())
    val rob_commit = Input(Vec(4, new Bundle {
      val valid = Bool()
      val old_preg = UInt(RegConfig.PHYS_REG_BITS.W)
    }))
  })

  // 寄存器别名表
  val RAT = RegInit(VecInit.tabulate(RegConfig.ARCH_REG_NUM)(i => 
    (i + RegConfig.ARCH_REG_NUM).U(RegConfig.PHYS_REG_BITS.W)))

  // 检查点存储
  val checkpointMem = SyncReadMem(1 << RegConfig.CHECKPOINT_DEPTH, new Bundle {
    val rat = Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W))
    val freeList = Vec(RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W))
    val freeHead = UInt((RegConfig.PHYS_REG_BITS + 1).W) // 8位
    val freeTail = UInt((RegConfig.PHYS_REG_BITS + 1).W) // 同步改为8位
  })

  // 空闲列表管理
  val freeList = RegInit(VecInit.tabulate(RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM)(i => 
    (i + RegConfig.ARCH_REG_NUM).U(RegConfig.PHYS_REG_BITS.W)))
  val freeValid = RegInit(VecInit(Seq.fill(RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM)(true.B)))
  val freeHead = RegInit(0.U((RegConfig.PHYS_REG_BITS + 1).W))
  val freeTail = RegInit((RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM - 1).U((RegConfig.PHYS_REG_BITS + 1).W))
  val freeCount = RegInit((RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM).U((RegConfig.PHYS_REG_BITS + 1).W))

  // 初始化逻辑
  when(reset.asBool) {
    freeHead := 0.U
    freeCount := (RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM).U
    for (i <- 0 until RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM) {
      freeList(i) := (i + RegConfig.ARCH_REG_NUM).U
      freeValid(i) := true.B
    }
  }

  // 重命名逻辑
  val allocPtrs = (0 until 4).map(i => (freeHead +& i.U) % (RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM).U)
  val canAlloc = freeCount >= 4.U
  io.alloc_ready := canAlloc

  for (i <- 0 until 4) {
    val rd = io.in(i).rd
    val needsAlloc = rd =/= 0.U && canAlloc
    
    val new_preg = freeList(allocPtrs(i))
    
    io.out(i).preg := Mux(needsAlloc, new_preg, 0.U)
    io.out(i).prj := Mux(io.in(i).rj.orR, RAT(io.in(i).rj), 0.U)
    io.out(i).prk := Mux(io.in(i).rk.orR, RAT(io.in(i).rk), 0.U)
    io.out(i).old_preg := Mux(rd.orR, RAT(rd), 0.U)

    when(needsAlloc) {
      RAT(rd) := new_preg
    }
  }

  when(io.alloc_ready) {
    freeHead := (freeHead +% 4.U) % (RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM).U
    freeCount := freeCount - 4.U
  }

  // 回收逻辑
  val recycleSlots = Wire(Vec(4, UInt((RegConfig.PHYS_REG_BITS + 1).W))) // 8-bit
  val recycleValid = Wire(Vec(4, Bool()))

  val MAX_FREE_INDEX = (RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM).U // 96.U(8.W)

  for (i <- 0 until 4) {
    val rawIndex = freeTail +& i.U
    recycleSlots(i) := Mux(rawIndex >= MAX_FREE_INDEX, 
                          rawIndex - MAX_FREE_INDEX,
                          rawIndex)

    val slotIndex = recycleSlots(i)(RegConfig.PHYS_REG_BITS-1, 0)
    recycleValid(i) := io.rob_commit(i).valid && 
                    !freeValid(slotIndex) && 
                    io.rob_commit(i).old_preg =/= 0.U
  }

  val canRecycle = PopCount(recycleValid)
  val newFreeTail = freeTail +& canRecycle
  freeTail := Mux(newFreeTail >= MAX_FREE_INDEX, 
                newFreeTail - MAX_FREE_INDEX,
                newFreeTail)

  freeCount := freeCount +% canRecycle

  for (i <- 0 until 4) {
    when(recycleValid(i) && (i.U < canRecycle)) {
      val slotIndex = recycleSlots(i)(RegConfig.PHYS_REG_BITS-1, 0)
      freeList(slotIndex) := io.rob_commit(i).old_preg
      freeValid(slotIndex) := true.B
    }
  }

  // 检查点保存
  when(io.checkpoint_save) {
    checkpointMem.write(io.checkpoint_id, {
      val data = Wire(chiselTypeOf(checkpointMem.read(0.U)))
      data.rat := RAT
      data.freeHead := freeHead
      data.freeList := freeList
      data.freeTail := freeTail
      data
    })
  }

  // 恢复逻辑
  val saved = checkpointMem.read(io.recover_chk, io.recover)
  when(io.recover) {
    RAT := saved.rat
    freeList := saved.freeList
    freeHead := saved.freeHead
    freeTail := saved.freeTail
  }

  val recoverDone = RegInit(false.B)
  recoverDone := io.recover && !recoverDone
  io.recover_done := recoverDone
}
