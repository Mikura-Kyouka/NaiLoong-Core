package core

import chisel3._
import chisel3.util._
import core.SrcType.reg
import core.IssueConfig.PHYS_REG_NUM
import os.makeDir.all

object RegConfig {
  val ARCH_REG_NUM = 32
  val PHYS_REG_NUM = 64
  val PHYS_REG_BITS = log2Ceil(PHYS_REG_NUM)
  val CHECKPOINT_DEPTH = 16
}

class Rename extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(4, new PipelineConnectIO)))
    val out = Decoupled(Vec(4, new PipelineConnectIO))
    val rob = Input(new RobCommit)
    val brMispredict = Input(new BrMisPredInfo)
    val exception = Input(Bool())
    val robAllocate = new RobAllocateIO
    val arf = Output(Vec(32, UInt(32.W))) // 逻辑寄存器
    val flush = Input(Bool())
  })
  
  val regRenaming = Module(new RegRenaming)
  io.arf := regRenaming.io.arf

  regRenaming.io.rob := io.rob
  regRenaming.io.robAllocate <> io.robAllocate

  regRenaming.io.in.valid := io.in.valid 
  io.in.ready := regRenaming.io.in.ready
  regRenaming.io.out.ready := io.out.ready
  io.out.valid := regRenaming.io.out.valid
  regRenaming.io.brMispredict := io.brMispredict
  regRenaming.io.exception := io.exception
  regRenaming.io.flush := io.flush

  for (i <- 0 until 4) {
    regRenaming.io.in.bits(i).ctrl := io.in.bits(i).ctrl
    regRenaming.io.in.bits(i).isBranch := io.in.bits(i).isBranch
    regRenaming.io.in.bits(i).checkpoint <> io.in.bits(i).checkpoint
    regRenaming.io.in.bits(i).pc := io.in.bits(i).pc         // 添加PC
    regRenaming.io.in.bits(i).instr := io.in.bits(i).instr   // 添加指令
    regRenaming.io.in.bits(i).inst_valid := io.in.bits(i).valid

    io.out.bits(i).prj := regRenaming.io.out.bits(i).prj
    io.out.bits(i).jIsArf := regRenaming.io.out.bits(i).jIsArf
    io.out.bits(i).dataj := regRenaming.io.out.bits(i).dataj
    io.out.bits(i).prk := regRenaming.io.out.bits(i).prk
    io.out.bits(i).kIsArf := regRenaming.io.out.bits(i).kIsArf
    io.out.bits(i).datak := regRenaming.io.out.bits(i).datak
    io.out.bits(i).preg := regRenaming.io.out.bits(i).preg
    io.out.bits(i).old_preg := regRenaming.io.out.bits(i).old_preg
    io.out.bits(i).checkpoint.needSave := regRenaming.io.out.bits(i).checkpoint.valid
    io.out.bits(i).checkpoint.id := regRenaming.io.out.bits(i).checkpoint.id
    io.out.bits(i).robIdx := regRenaming.io.out.bits(i).robIdx   // 添加ROB索引

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
    io.out.bits(i).csrNewData := io.in.bits(i).csrNewData
    io.out.bits(i).redirect := io.in.bits(i).redirect
  }
}

class RenameInput extends Bundle {
  val ctrl = new CtrlSignalIO
  val isBranch = Bool()
  val checkpoint = new Bundle {
    val needSave = Bool()
    val id = UInt(64.W)
  }
  val pc = UInt(32.W)       // 添加PC
  val instr = UInt(32.W)    // 添加指令
  val inst_valid = Bool()
}

class RenameOutput extends Bundle {
  val prj      = UInt(RegConfig.PHYS_REG_BITS.W)
  val jIsArf   = Bool()
  val dataj    = UInt(32.W)
  val prk      = UInt(RegConfig.PHYS_REG_BITS.W)
  val kIsArf   = Bool()
  val datak    = UInt(32.W)
  val preg     = UInt(RegConfig.PHYS_REG_BITS.W)
  val old_preg = UInt(RegConfig.PHYS_REG_BITS.W)
  val checkpoint = new Bundle {
    val valid = Bool()
    val id    = UInt(RegConfig.CHECKPOINT_DEPTH.W)
  }
  val robIdx   = UInt(RobConfig.ROB_INDEX_WIDTH.W) // ROB索引
}

class aliasTableEntry extends Bundle {
  val inARF      = Bool() // if true, read arf; else read rob(robPointer)
  val preg       = UInt(RegConfig.PHYS_REG_BITS.W)
  val robPointer = UInt(RobConfig.ROB_INDEX_WIDTH.W)
}

class RegRenaming extends Module {
  val io = IO(new Bundle {
    val in           = Flipped(Decoupled(Vec(4, new RenameInput)))
    val out          = Decoupled(Vec(4, new RenameOutput))
    val rob          = Input(new RobCommit)
    val brMispredict = Input(new BrMisPredInfo)
    val exception    = Input(Bool())
    val robAllocate  = new RobAllocateIO
    val arf = Output(Vec(32, UInt(32.W))) // 逻辑寄存器
    val flush = Input(Bool())
  })

  val arf = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  io.arf := arf
  // 寄存器别名表
  // val rat = RegInit(VecInit.tabulate(RegConfig.ARCH_REG_NUM)(i => 
  //   (i + RegConfig.ARCH_REG_NUM).U(RegConfig.PHYS_REG_BITS.W)))
  val rat = RegInit(VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
    val entry = Wire(new aliasTableEntry)
    entry.inARF := true.B
    entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
    entry.robPointer := 0.U(RobConfig.ROB_INDEX_WIDTH.W)
    entry
  }))

  var emptyRat = RegInit(VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
    val entry = Wire(new aliasTableEntry)
    entry.inARF := true.B
    entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
    entry.robPointer := 0.U(RobConfig.ROB_INDEX_WIDTH.W)
    entry
  }))

  // Define a Vec type for the checkpoint RAT
  val checkpointRATEntryType = Vec(RegConfig.ARCH_REG_NUM, new aliasTableEntry)
  val checkpointRAT = RegInit(VecInit(Seq.fill(RegConfig.CHECKPOINT_DEPTH) {
    VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
      val entry = Wire(new aliasTableEntry)
      entry.inARF := true.B
      entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
      entry.robPointer := 0.U(RobConfig.ROB_INDEX_WIDTH.W)
      entry
    })
  }))

  // Store the state of the freelist for checkpoints
  class FreeListState extends Bundle {
    val head = UInt((RegConfig.PHYS_REG_BITS + 1).W)
    val tail = UInt((RegConfig.PHYS_REG_BITS + 1).W)
  }

  val checkpointFreelist = RegInit(VecInit(Seq.fill(RegConfig.CHECKPOINT_DEPTH)(0.U.asTypeOf(new FreeListState))))

  // 物理寄存器空闲队列
  class FreeList extends Module {
    val io = IO(new Bundle {
      val allocReq  = Vec(4, Flipped(Valid(UInt(RegConfig.PHYS_REG_BITS.W))))
      val allocResp = Vec(4, DecoupledIO(UInt(RegConfig.PHYS_REG_BITS.W)))
      val free      = Flipped(Vec(RobConfig.ROB_CMT_NUM, ValidIO(UInt(RegConfig.PHYS_REG_BITS.W))))
      val count     = Output(UInt((RegConfig.PHYS_REG_BITS + 1).W))
      val rollback  = Input(Valid(new FreeListState))
      val flHead    = Output(UInt((RegConfig.PHYS_REG_BITS + 1).W))
      val flTail    = Output(UInt((RegConfig.PHYS_REG_BITS + 1).W))
    })
    // 寄存器初始化
    val entries = RegInit(VecInit((0 until RegConfig.PHYS_REG_NUM).map(_.U)))
    val head = RegInit(1.U((RegConfig.PHYS_REG_BITS + 1).W))
    val tail = RegInit((RegConfig.PHYS_REG_NUM - 1).U((RegConfig.PHYS_REG_BITS + 1).W))

    io.flHead := head
    io.flTail := tail

    // 分配有效性校验
    val reqValid = Wire(Vec(4, Bool()))
    val reqReady = Wire(Vec(4, Bool()))
    val allocIndexes = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
    val respValid = Wire(Vec(4, Bool()))
    val cnt = Wire(Vec(4, UInt(2.W)))

    for (i <- 0 until 4) {
      reqValid(i) := io.allocReq(i).valid
      reqReady(i) := io.count >= (i + 1).U
      respValid(i) := reqValid(i) && reqReady(i)
    }

    cnt(0) := 0.U
    cnt(1) := respValid(0).asUInt
    cnt(2) := PopCount(VecInit(respValid(0), respValid(1)))
    cnt(3) := PopCount(VecInit(respValid(0), respValid(1), respValid(2)))

    for (i <- 0 until 4) {
      allocIndexes(i) := (head +& cnt(i)) % entries.size.U
    }

    val finalAllocIndexes = WireInit(allocIndexes)

    // 如果分配的索引为0，则将其和其后的加1
    val allocIndexesHasZeroVec = VecInit(Seq.fill(4)(false.B))
    val allocZeroIndex = WireInit(0.U(2.W))
    for (i <- 0 until 4) {
      allocIndexesHasZeroVec(i) := allocIndexes(i) === 0.U && respValid(i)
    }
    val allocIndexesHasZero = allocIndexesHasZeroVec.reduce(_ || _)
    when(allocIndexesHasZero) {
      allocZeroIndex := PriorityEncoder(allocIndexesHasZeroVec)
    }
    when(allocIndexesHasZero) {
      for (i <- 0 until 4) {
        when(respValid(i) && i.asUInt >= allocZeroIndex) {
          finalAllocIndexes(i) := allocIndexes(i) +& 1.U
        }
      }
    }

    // 并行分配逻辑
    val allocCnt = PopCount(reqValid.zip(reqReady).map { case (v, r) => v && r })
    
    // 响应生成
    for (i <- 0 until 4) {
      io.allocResp(i).valid := reqValid(i) && reqReady(i)
      io.allocResp(i).bits := entries(finalAllocIndexes(i))
    }

    // Head指针更新
    when(io.allocResp.map(_.valid).reduce(_||_)) {
      head := Mux(head +& allocCnt >= entries.size.U,
                head +& allocCnt - entries.size.U + 1.U,
                head +& allocCnt)
    }

    // 回收逻辑
    val freeValidCount = PopCount(io.free.map(_.valid))
    when(freeValidCount > 0.U) {
      // tail := (tail +& freeValidCount) % entries.size.U
      tail := Mux(tail +& freeValidCount >= entries.size.U,
                tail +& freeValidCount - entries.size.U + 1.U,
                tail +& freeValidCount)
    }

    io.count := Mux(tail >= head, tail - head, (entries.size.U - head) + tail - 1.U)  

    when(io.rollback.valid) {
      head := io.rollback.bits.head % entries.size.U
      tail := io.rollback.bits.tail % entries.size.U
    }
  }

  val freeList = Module(new FreeList)

  // 准备发送到ROB的分配信息
  for (i <- 0 until 4) {
    val entry = io.robAllocate.allocEntries(i)
    val input = io.in.bits(i)
    val rd = input.ctrl.rfDest
    val rfWen = input.ctrl.rfWen
    
    entry.pc := input.pc
    entry.instr := input.instr
    entry.rd := rd
    entry.rfWen := rfWen
    entry.isBranch := input.isBranch
    entry.isStore := (input.ctrl.fuType === FuType.lsu && LSUOpType.isStore(input.ctrl.fuOpType))
    entry.checkpoint.valid := input.checkpoint.needSave
    entry.checkpoint.id := input.checkpoint.id
    entry.fuType := input.ctrl.fuType
    entry.inst_valid := input.inst_valid
    entry.csrOp := input.ctrl.csrOp
    entry.csrNum := input.ctrl.csrNum
    
    // 这些字段在后面的指令执行阶段设置
    entry.finished := DontCare
    entry.exception := DontCare
    entry.exceptionVec := DontCare
    entry.eret := DontCare
    entry.intrVec := DontCare
    entry.brMispredict := DontCare
    entry.brTarget := DontCare
    entry.result := DontCare
    entry.csrNewData := DontCare
    entry.timer64 := DontCare
    entry.brTaken := DontCare
    entry.tlbInfo := DontCare
  }

  // 连接到ROB分配接口
  io.robAllocate.allocReq := io.in.valid && io.out.ready
  io.robAllocate.allocCount := PopCount(io.in.bits.map(x => io.in.valid && x.inst_valid))
  
  // 握手信号控制
  val canAlloc = freeList.io.count >= 4.U && io.robAllocate.canAllocate
  io.in.ready := canAlloc && io.out.ready
  dontTouch(canAlloc)
  io.out.valid := io.in.valid && canAlloc

  val temp_prj = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
  val temp_prk = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
  val temp_jIsArf = Wire(Vec(4, Bool()))
  val temp_kIsArf = Wire(Vec(4, Bool()))

  val allocated_preg = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))

  val renameFired = Wire(Vec(4, Bool()))

  // 寄存器重命名
  for (i <- 0 until 4) {
    val input = io.in.bits(i)
    val rj = input.ctrl.rfSrc1
    val rk = input.ctrl.rfSrc2
    val rd = input.ctrl.rfDest
    val rfWen = input.ctrl.rfWen// 0有效
    val instValid = input.inst_valid
    val isZeroReg = (rd === 0.U)

    // 目标寄存器分配
    val needAlloc = !rfWen && !isZeroReg && instValid
    freeList.io.allocReq(i).valid := needAlloc && io.in.valid && io.out.ready
    freeList.io.allocReq(i).bits := DontCare
    freeList.io.allocResp(i).ready := needAlloc && io.in.valid && io.out.ready
    
    allocated_preg(i) := Mux(needAlloc && freeList.io.allocResp(i).valid,
                             freeList.io.allocResp(i).bits,
                             0.U)
    io.out.bits(i).preg := Mux(instValid, allocated_preg(i), 0.U)

    // 更新ROB条目中的物理寄存器信息
    io.robAllocate.allocEntries(i).preg := allocated_preg(i)
    io.robAllocate.allocEntries(i).old_preg := Mux(rd.orR, rat(rd).robPointer, 0.U)
    // FIXME:
    io.robAllocate.allocEntries(i).valid := needAlloc && freeList.io.allocResp(i).valid
    io.robAllocate.allocEntries(i).use_preg := needAlloc && freeList.io.allocResp(i).valid

    // 更新RAT
    when(io.in.valid && needAlloc && freeList.io.allocResp(i).valid && io.in.bits(i).inst_valid) {
      rat(rd).inARF := false.B
      rat(rd).preg := allocated_preg(i)
      rat(rd).robPointer := freeList.io.allocResp(i).bits
    }
    renameFired(i) := io.in.valid && needAlloc && freeList.io.allocResp(i).valid && io.in.bits(i).inst_valid

    // 源寄存器映射
    temp_prj(i) := Mux(rj.orR, rat(rj).preg, 0.U)
    temp_jIsArf(i) := rat(rj).inARF
    io.out.bits(i).dataj := arf(rj)

    temp_prk(i) := Mux((input.ctrl.src2Type === SrcType.reg || input.ctrl.srcIsRd === SrcIsRd.y) && rk.orR,
                            rat(rk).preg, 
                            0.U)
    temp_kIsArf(i) := rat(rk).inARF
    io.out.bits(i).datak := arf(rk)
    
    // 立即数不需要物理寄存器
    when(input.ctrl.src2Type === SrcType.imm && input.ctrl.srcIsRd === SrcIsRd.n) {
      temp_prk(i) := 0.U 
    }

    // 检查点处理
    io.out.bits(i).checkpoint.valid := input.checkpoint.needSave
    io.out.bits(i).checkpoint.id := input.checkpoint.id

    io.out.bits(i).old_preg := Mux(rd.orR, rat(rd).preg, 0.U)
    
    // 分配ROB索引
    io.out.bits(i).robIdx := io.robAllocate.allocResp(i)

    // for load/store difftest
    io.robAllocate.allocEntries(i).paddr := DontCare
    io.robAllocate.allocEntries(i).wdata := DontCare
    io.robAllocate.allocEntries(i).optype := DontCare
  }

  // 组内相关性处理
  io.out.bits(0).prj := temp_prj(0)
  io.out.bits(0).jIsArf := temp_jIsArf(0)
  io.out.bits(0).prk := temp_prk(0)
  io.out.bits(0).kIsArf := temp_kIsArf(0)

  for (i <- 0 until 4) {
    val input = io.in.bits(i)
    val rj = input.ctrl.rfSrc1
    val rk = input.ctrl.rfSrc2

    io.out.bits(i).prj := temp_prj(i)
    io.out.bits(i).prk := temp_prk(i)
    io.out.bits(i).jIsArf := temp_jIsArf(i)
    io.out.bits(i).kIsArf := temp_kIsArf(i)

    for (j <- 0 until i) {
      when(rj.orR && rj === io.in.bits(j).ctrl.rfDest && io.in.bits(j).inst_valid && io.in.bits(j).ctrl.rfWen === RfWen.y) {
        io.out.bits(i).prj := allocated_preg(j)
        io.out.bits(i).jIsArf := false.B
      }
      when(rk.orR && rk === io.in.bits(j).ctrl.rfDest && io.in.bits(j).inst_valid && io.in.bits(j).ctrl.rfWen === RfWen.y) {
        io.out.bits(i).prk := allocated_preg(j)
        io.out.bits(i).kIsArf := false.B
      }
    }
  }

  // 存储检查点
  for (i <- 0 until 4) {
    val input = io.in.bits(i)
    when(input.checkpoint.needSave && io.in.valid) {
      checkpointRAT(input.checkpoint.id) := rat
      val allocUntilCheckpointValid = PopCount(VecInit((0 until i + 1).map(j => 
        io.robAllocate.allocEntries(j).use_preg
      )))
      checkpointFreelist(input.checkpoint.id).head := (freeList.io.flHead +& allocUntilCheckpointValid) % 64.U
      checkpointFreelist(input.checkpoint.id).tail := (freeList.io.flTail +& PopCount(freeList.io.free.map(_.valid))) % 64.U
    }
  }
  
  // 零寄存器不保留旧值
  io.out.bits.foreach { out =>
    when(out.preg === 0.U) {
      out.old_preg := 0.U
    }
  }
  
  io.robAllocate.allocEntries.foreach { entry =>
    when(entry.preg === 0.U) {
      entry.old_preg := 0.U
    }
  }

  // 连接ROB回收接口
  freeList.io.free.zip(io.rob.commit).foreach { case (free, commit) =>
    free.valid := commit.bits.use_preg && commit.valid
    free.bits  := commit.bits.preg
  }

  // 分支预测错误/异常回滚
  when(io.flush) {
    // val checkpoint_id = io.brMispredict.brMisPredChkpt
    // val ratSnapshot = checkpointRAT(checkpoint_id)
    // rat := ratSnapshot
    rat := emptyRat

    // val freelistSnapshot = checkpointFreelist(checkpoint_id)
    freeList.io.rollback.bits.head := 1.U
    freeList.io.rollback.bits.tail := 63.U
  }.otherwise {
    freeList.io.rollback.bits := DontCare
  }
  freeList.io.rollback.valid := io.flush

  // retire 
  for(i <- 0 until RobConfig.ROB_CMT_NUM) {
    when(io.rob.commit(i).valid && io.rob.commit(i).bits.inst_valid) {
      // write arf 
      arf(io.rob.commit(i).bits.dest) := io.rob.commit(i).bits.data
      // rat update
      // FIXME: 采用io.brMispredict.brMisPred.valid这么简单粗暴的方式大概率有错误
      when (io.rob.commit(i).bits.preg === rat(io.rob.commit(i).bits.dest).preg && 
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(0).rd && renameFired(0)) &&
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(1).rd && renameFired(1)) &&
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(2).rd && renameFired(2)) &&
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(3).rd && renameFired(3))) {
        rat(io.rob.commit(i).bits.dest).inARF := true.B
      }
    }
  }
}