package core

import chisel3._
import chisel3.util._

object RegConfig {
  val ARCH_REG_NUM = 32
  val PHYS_REG_NUM = 128
  val PHYS_REG_BITS = log2Ceil(PHYS_REG_NUM)
  val FREE_LIST_SIZE = PHYS_REG_NUM - ARCH_REG_NUM
  val FREE_LIST_BITS = log2Ceil(FREE_LIST_SIZE + 1)
}

class DispatchEntry extends Bundle {
  val pc = UInt(32.W)
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val preg = UInt(6.W)
  val opreg = UInt(6.W)
  val instType = UInt(3.W)
  val checkpoint = UInt(6.W)
}

class RenameCheckpoint extends Bundle {
  val rmt = Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W))
  val freeHead = UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)
  val freeTail = UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)
  val freeCount = UInt(RegConfig.FREE_LIST_BITS.W)
}

class RegRenamingIO extends Bundle {
  val from = Vec(4, Flipped(Decoupled(new DispatchEntry)))
  val to = Vec(4, Decoupled(new DispatchEntry))

  val commit_free_valid = Input(Vec(4, Bool()))
  val commit_free_preg = Input(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))

  val recover = Input(Bool())
  val recover_chk = Input(UInt(6.W))
  val recover_done = Output(Bool())

  val free_list_count = Output(UInt(RegConfig.FREE_LIST_BITS.W))
  val ren_alloc_ready = Output(Bool())
}

class RegRenaming extends Module {
  val io = IO(new RegRenamingIO)

  val rmt = RegInit(VecInit(Seq.tabulate(RegConfig.ARCH_REG_NUM) { i =>
    i.U(RegConfig.PHYS_REG_BITS.W)
  }))

  val freeList = RegInit(
    VecInit(
      (RegConfig.ARCH_REG_NUM until RegConfig.PHYS_REG_NUM)
        .map(i => i.U(RegConfig.PHYS_REG_BITS.W))
    )
  )
  val freeHead = RegInit(0.U(log2Ceil(RegConfig.FREE_LIST_SIZE).W))
  val freeTail = RegInit(0.U(log2Ceil(RegConfig.FREE_LIST_SIZE).W))
  val freeCount = RegInit(RegConfig.FREE_LIST_SIZE.U(RegConfig.FREE_LIST_BITS.W))

  val numCheckpoints = 64
  val chkRmt = Reg(Vec(numCheckpoints, Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W))))
  val chkFreeHead = Reg(Vec(numCheckpoints, UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)))
  val chkFreeTail = Reg(Vec(numCheckpoints, UInt(log2Ceil(RegConfig.FREE_LIST_SIZE).W)))
  val chkFreeCount = Reg(Vec(numCheckpoints, UInt(RegConfig.FREE_LIST_BITS.W)))
  val chkValid = RegInit(VecInit(Seq.fill(numCheckpoints)(false.B)))

  val nextChkId = RegInit(0.U(6.W))

  io.recover_done := false.B

  val allocCount = PopCount(io.from.map(_.valid))
  val ren_alloc_ready = freeCount >= allocCount
  io.ren_alloc_ready := ren_alloc_ready

  for (i <- 0 until 4) {
    io.from(i).ready := io.to(i).ready && ren_alloc_ready
    io.to(i).valid := io.from(i).valid && ren_alloc_ready
    io.to(i).bits := io.from(i).bits

    io.to(i).bits.preg := freeList(freeHead)
    io.to(i).bits.rj := rmt(io.from(i).bits.rj)
    io.to(i).bits.rk := rmt(io.from(i).bits.rk)

    when(io.from(i).valid && ren_alloc_ready) {
      rmt(io.from(i).bits.rd) := freeList(freeHead)
      freeHead := freeHead + 1.U
      freeCount := freeCount - 1.U
    }
  }

  val commitFreeCount = PopCount(io.commit_free_valid)
  when(commitFreeCount > 0.U) {
    for (i <- 0 until 4) {
      when(io.commit_free_valid(i)) {
        freeList(freeTail) := io.commit_free_preg(i)
        freeTail := freeTail + 1.U
      }
    }
    freeCount := freeCount + commitFreeCount
  }

  when(io.recover) {
    when(chkValid(io.recover_chk)) {
      for (i <- 0 until RegConfig.ARCH_REG_NUM) {
        rmt(i) := chkRmt(io.recover_chk)(i)
      }
      freeHead := chkFreeHead(io.recover_chk)
      freeTail := chkFreeTail(io.recover_chk)
      freeCount := chkFreeCount(io.recover_chk)
      chkValid(io.recover_chk) := false.B
      io.recover_done := true.B
    }.otherwise {
      io.recover_done := false.B
    }
  }

  io.free_list_count := freeCount
}
