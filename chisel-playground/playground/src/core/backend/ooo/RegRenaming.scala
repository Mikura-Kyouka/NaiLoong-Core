package core

import chisel3._
import chisel3.util._
import core.SrcType.reg

object RegConfig {
  val ARCH_REG_NUM = 32
  val PHYS_REG_NUM = 128
  val PHYS_REG_BITS = log2Ceil(PHYS_REG_NUM)
  val CHECKPOINT_DEPTH = 8
}

class Rename extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(4, new PipelineConnectIO)))
    val out = Decoupled(Vec(4, new PipelineConnectIO))
    val rob = Input(new RobCommit)
  })
  
  val regRenaming = Module(new RegRenaming)

  regRenaming.io.rob := io.rob

  regRenaming.io.in.valid := io.in.valid 
  io.in.ready := regRenaming.io.in.ready
  regRenaming.io.out.ready := io.out.ready
  io.out.valid := regRenaming.io.out.valid

  for (i <- 0 until 4) {
    regRenaming.io.in.bits(i).ctrl := io.in.bits(i).ctrl
    regRenaming.io.in.bits(i).isBranch := io.in.bits(i).isBranch
    regRenaming.io.in.bits(i).checkpoint <> io.in.bits(i).checkpoint

    io.out.bits(i).prj := regRenaming.io.out.bits(i).prj
    io.out.bits(i).prk := regRenaming.io.out.bits(i).prk
    io.out.bits(i).preg := regRenaming.io.out.bits(i).preg
    io.out.bits(i).old_preg := regRenaming.io.out.bits(i).old_preg
    io.out.bits(i).checkpoint.needSave := regRenaming.io.out.bits(i).checkpoint.valid
    io.out.bits(i).checkpoint.id := regRenaming.io.out.bits(i).checkpoint.id

    io.out.bits(i).instr := io.in.bits(i).instr
    io.out.bits(i).pc := io.in.bits(i).pc
    io.out.bits(i).pnpc := io.in.bits(i).pnpc
    io.out.bits(i).redirect := io.in.bits(i).redirect
    io.out.bits(i).exceptionVec := io.in.bits(i).exceptionVec
    io.out.bits(i).intrVec := io.in.bits(i).intrVec
    io.out.bits(i).brIdx := io.in.bits(i).brIdx
    io.out.bits(i).crossPageIPFFix := io.in.bits(i).crossPageIPFFix
    io.out.bits(i).isBranch := io.in.bits(i).isBranch
    io.out.bits(i).src1 := io.in.bits(i).src1
    io.out.bits(i).src2 := io.in.bits(i).src2
    io.out.bits(i).imm := io.in.bits(i).imm
    io.out.bits(i).ctrl := io.in.bits(i).ctrl
    io.out.bits(i).valid := io.in.bits(i).valid
  }
}

class RenameInput extends Bundle {
  val ctrl = new CtrlSignalIO
  val isBranch = Bool()
  val checkpoint = new Bundle {
    val needSave = Bool()
    val id = UInt(64.W)
  }
}

class RenameOutput extends Bundle {
  val prj      = UInt(RegConfig.PHYS_REG_BITS.W)
  val prk      = UInt(RegConfig.PHYS_REG_BITS.W)
  val preg     = UInt(RegConfig.PHYS_REG_BITS.W)
  val old_preg = UInt(RegConfig.PHYS_REG_BITS.W)
  val checkpoint = new Bundle {
    val valid = Bool()
    val id    = UInt(RegConfig.CHECKPOINT_DEPTH.W)
  }
}

class RobCommit extends Bundle {
  val commit = Vec(4, Valid(UInt(RegConfig.PHYS_REG_BITS.W)))
}

class RegRenaming extends Module {
  val io = IO(new Bundle {
    val in      = Flipped(Decoupled(Vec(4, new RenameInput)))
    val out     = Decoupled(Vec(4, new RenameOutput))
    val rob     = Input(new RobCommit)
  })

  // 寄存器别名表
  val rat = RegInit(VecInit.tabulate(RegConfig.ARCH_REG_NUM)(i => 
    (i + RegConfig.ARCH_REG_NUM).U(RegConfig.PHYS_REG_BITS.W)))
  val checkpointRAT = SyncReadMem(1 << RegConfig.CHECKPOINT_DEPTH, 
    Vec(RegConfig.ARCH_REG_NUM, UInt(RegConfig.PHYS_REG_BITS.W)))

  // 物理寄存器空闲队列
  class FreeList extends Module {
    val io = IO(new Bundle {
      val alloc  = Vec(4, DecoupledIO(UInt(RegConfig.PHYS_REG_BITS.W)))
      val free   = Flipped(Vec(4, ValidIO(UInt(RegConfig.PHYS_REG_BITS.W))))
      val count  = Output(UInt((RegConfig.PHYS_REG_BITS + 1).W))
    })
    
    val entries = RegInit(VecInit((RegConfig.ARCH_REG_NUM until RegConfig.PHYS_REG_NUM).map(_.U)))
    val head    = RegInit(0.U((RegConfig.PHYS_REG_BITS + 1).W))
    val tail    = RegInit((RegConfig.PHYS_REG_NUM - RegConfig.ARCH_REG_NUM).U((RegConfig.PHYS_REG_BITS + 1).W))
    
    // 四路并行分配
    for (i <- 0 until 4) {
      val canAlloc = (tail - head) > i.U
      val index = (head + i.U) % entries.size.U
      io.alloc(i).bits  := Mux(canAlloc, entries(index), 0.U)
      io.alloc(i).valid := canAlloc
    }

    // 分配成功后更新head
    val allocFireCnt = PopCount(io.alloc.map(_.fire))
    when(io.alloc(0).fire) {
      head := Mux(head +& allocFireCnt >= entries.size.U, 
                head + allocFireCnt - entries.size.U, 
                head + allocFireCnt)
    }
    
    // 四路并行回收
    for (i <- 0 until 4) {
      when(io.free(i).valid) {
        val index = tail % entries.size.U
        entries(index) := io.free(i).bits
        tail := tail +% 1.U
      }
    }

    io.count := Mux(tail >= head, tail - head, (entries.size.U - head) + tail)
  }

  val freeList = Module(new FreeList)
  for (i <- 0 until 4) {
    freeList.io.alloc(i).ready := true.B
  }

  // 握手信号控制
  io.in.ready := freeList.io.count >= 4.U
  io.out.valid := io.in.valid

  // 重命名逻辑
  for (i <- 0 until 4) {
    val input = io.in.bits(i)
    val rj = input.ctrl.rfSrc1
    val rk = input.ctrl.rfSrc2
    val rd = input.ctrl.rfDest
    val rfWen = input.ctrl.rfWen
    val isZeroReg = (rd === 0.U)

    // 源寄存器映射
    io.out.bits(i).prj := Mux(rj.orR, rat(rj), 0.U)
    io.out.bits(i).prk := Mux(input.ctrl.src2Type === SrcType.reg && rk.orR, 
                            rat(rk), 
                            0.U)
    
    // 目标寄存器分配
    val needAlloc = rfWen && !isZeroReg && freeList.io.alloc(i).valid
    io.out.bits(i).preg := Mux(needAlloc, 
                              freeList.io.alloc(i).bits, 
                              0.U)

    // 更新RAT
    when(io.in.fire && needAlloc) {
      rat(rd) := freeList.io.alloc(i).bits
    }

    // 立即数不需要物理寄存器
    when(input.ctrl.src2Type === SrcType.imm) {
      io.out.bits(i).prk := 0.U 
    }

    // 检查点处理
    when(input.checkpoint.needSave && io.in.fire) {
      checkpointRAT.write(input.checkpoint.id, rat)
    }
    io.out.bits(i).checkpoint.valid := input.checkpoint.needSave
    io.out.bits(i).checkpoint.id := input.checkpoint.id

    io.out.bits(i).old_preg := Mux(rd.orR, rat(rd), 0.U)
  }
  
  // 零寄存器不保留旧值
  io.out.bits.foreach { out =>
    when(out.preg === 0.U) {
      out.old_preg := 0.U
    }
  }

  // 连接ROB回收接口
  freeList.io.free.zip(io.rob.commit).foreach { case (free, commit) =>
    free.valid := commit.valid
    free.bits  := commit.bits
  }

}