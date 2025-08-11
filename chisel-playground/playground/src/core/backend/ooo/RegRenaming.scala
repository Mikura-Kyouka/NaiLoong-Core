package core

import chisel3._
import chisel3.util._
import core.SrcType.reg
import core.IssueConfig.PHYS_REG_NUM
import utils.PipelineConnect

object RegConfig {
  val ARCH_REG_NUM = 32
  val PHYS_REG_NUM = 64
  val PHYS_REG_BITS = log2Ceil(PHYS_REG_NUM)
  val CHECKPOINT_DEPTH = 48
  val CHECKPOINT_BITS = log2Ceil(CHECKPOINT_DEPTH)
}

class Rename extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(4, new PipelineConnectIO)))
    val out = Decoupled(Vec(4, new PipelineConnectIO))
    val s1Fire = Output(Bool())
    val rob = Input(new RobCommit)
    val brMispredict = Input(new BrMisPredInfo)
    val robAllocate = new RobAllocateIO
    val arf = Output(Vec(32, UInt(32.W))) // 逻辑寄存器
    val flush = Input(Bool())
    // val rollbackChkpt = Input(UInt(RegConfig.CHECKPOINT_BITS.W)) // 回滚检查点ID
  })
  
  val regRenaming1 = Module(new RegRenaming1)
  val regRenaming = Module(new RegRenaming)
  io.arf := regRenaming.io.arf

  io.s1Fire := regRenaming1.io.out.fire
  
  PipelineConnect(regRenaming1.io.out, regRenaming.io.in, regRenaming.io.out.fire, io.flush)
  PipelineConnect(regRenaming1.io.preg, regRenaming.io.preg, regRenaming.io.preg.fire, io.flush)

  regRenaming.io.rob := io.rob
  regRenaming1.io.rob := io.rob
  regRenaming.io.robAllocate <> io.robAllocate

  regRenaming1.io.in.valid := io.in.valid 
  io.in.ready := regRenaming1.io.in.ready
  regRenaming.io.out.ready := io.out.ready
  io.out.valid := regRenaming.io.out.valid
  regRenaming.io.brMispredict := io.brMispredict
  regRenaming.io.flush := io.flush
  regRenaming1.io.flush := io.flush
  // regRenaming.io.rollbackChkpt := io.rollbackChkpt

  for (i <- 0 until 4) {
    regRenaming1.io.in.bits(i).ctrl := io.in.bits(i).ctrl
    regRenaming1.io.in.bits(i).isBranch := io.in.bits(i).isBranch
    regRenaming1.io.in.bits(i).checkpoint <> io.in.bits(i).checkpoint
    regRenaming1.io.in.bits(i).pc := io.in.bits(i).pc 
    regRenaming1.io.in.bits(i).instr := io.in.bits(i).instr
    regRenaming1.io.in.bits(i).inst_valid := io.in.bits(i).valid

    regRenaming1.io.in.bits(i).other := io.in.bits(i)

    io.out.bits(i) := regRenaming.io.out.bits(i).other

    io.out.bits(i).prj := regRenaming.io.out.bits(i).prj
    io.out.bits(i).jIsArf := regRenaming.io.out.bits(i).jIsArf
    io.out.bits(i).dataj := regRenaming.io.out.bits(i).dataj
    io.out.bits(i).prk := regRenaming.io.out.bits(i).prk
    io.out.bits(i).kIsArf := regRenaming.io.out.bits(i).kIsArf
    io.out.bits(i).datak := regRenaming.io.out.bits(i).datak
    io.out.bits(i).preg := regRenaming.io.out.bits(i).preg
    io.out.bits(i).old_preg := DontCare
    io.out.bits(i).checkpoint.needSave := regRenaming.io.out.bits(i).checkpoint.valid
    io.out.bits(i).checkpoint.id := regRenaming.io.out.bits(i).checkpoint.id
    io.out.bits(i).robIdx := regRenaming.io.out.bits(i).robIdx   // 添加ROB索引
  }
}

  class FreeListState extends Bundle {
    val head = UInt((RegConfig.PHYS_REG_BITS + 1).W)
    val tail = UInt((RegConfig.PHYS_REG_BITS + 1).W)
  }

  // 物理寄存器空闲队列
  class FreeList extends Module {
    val io = IO(new Bundle {
      val allocReq  = Vec(4, Flipped(Valid(UInt(RegConfig.PHYS_REG_BITS.W))))
      val allocResp = Vec(4, DecoupledIO(UInt(RegConfig.PHYS_REG_BITS.W)))
      val fire      = Input(Bool())
      val free      = Flipped(Vec(RobConfig.ROB_CMT_NUM, ValidIO(UInt(RegConfig.PHYS_REG_BITS.W))))
      val count     = Output(UInt((RegConfig.PHYS_REG_BITS + 1).W))
      val rollback  = Input(Valid(new FreeListState))
      val flHead    = Output(UInt((RegConfig.PHYS_REG_BITS + 1).W))
      val flTail    = Output(UInt((RegConfig.PHYS_REG_BITS + 1).W))
    })
    // 寄存器初始化
    val entries = RegInit(VecInit((0 until RegConfig.PHYS_REG_NUM).map(_.U)))
    // val entryUsed = RegInit(VecInit(Seq.fill(RegConfig.PHYS_REG_NUM)(false.B)))
    // val entryUsedCount = PopCount(entryUsed)
    val head = RegInit(1.U((RegConfig.PHYS_REG_BITS + 1).W))
    val tail = RegInit(1.U((RegConfig.PHYS_REG_BITS + 1).W))
    val full = RegInit(false.B)

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
      reqReady(i) := (io.count > PopCount(reqValid)) || io.count === 0.U && !full
      respValid(i) := reqValid(i) && reqReady(i)
    }

    cnt(0) := 0.U
    cnt(1) := respValid(0).asUInt
    cnt(2) := PopCount(VecInit(respValid(0), respValid(1)))
    cnt(3) := PopCount(VecInit(respValid(0), respValid(1), respValid(2)))

    for (i <- 0 until 4) {
      allocIndexes(i) := (head +& cnt(i)) % entries.size.U
    }

    // 并行分配逻辑
    val allocCnt = PopCount(reqValid.zip(reqReady).map { case (v, r) => v && r })
    
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

    // 响应生成
    for (i <- 0 until 4) {
      io.allocResp(i).valid := reqValid(i) && reqReady(i)
      io.allocResp(i).bits := entries(finalAllocIndexes(i))
    }

    // Head指针更新
    when(io.allocResp.map(_.valid).reduce(_||_) && io.fire) {
      val newHead = Mux(head +& allocCnt >= entries.size.U,
                    head +& allocCnt - entries.size.U + 1.U,
                    head +& allocCnt)
      head := newHead
      when(newHead === tail) {
        full := true.B
      }
    }
    
    when(head =/= tail || io.rollback.valid) {
      full := false.B
    }

    val freeValid = io.free.map(_.valid)
    val freeCount = PopCount(freeValid)

    when(freeValid.reduce(_ || _)) {
      val prefixSum = Wire(Vec(RobConfig.ROB_CMT_NUM, UInt(3.W)))
      prefixSum(0) := 0.U
      for (j <- 1 until RobConfig.ROB_CMT_NUM) {
        prefixSum(j) := prefixSum(j-1) + freeValid(j-1).asUInt
      }

      for (i <- 0 until RobConfig.ROB_CMT_NUM) {
        when(freeValid(i) && prefixSum(i) < freeCount) {
          val freeIndex = (tail +& prefixSum(i)) % entries.size.U
          val finalFreeIndex = Mux(freeIndex === 0.U, 1.U, freeIndex)
          entries(finalFreeIndex) := io.free(i).bits
          // entryUsed(freeIndex) := false.B
        }
      }
    }

    tail := Mux(tail +& freeCount >= entries.size.U,
                tail +& freeCount - entries.size.U + 1.U,
                tail +& freeCount)

    io.count := Mux(tail === head && !full, entries.size.U, 
                Mux(tail >= head, tail - head, (entries.size.U - head) + tail - 1.U))  

    when(io.rollback.valid) {
      head := io.rollback.bits.head % entries.size.U
      tail := io.rollback.bits.tail % entries.size.U
      entries := VecInit((0 until RegConfig.PHYS_REG_NUM).map(_.U))
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
  val other = new PipelineConnectIO
}

class RenameOutput extends Bundle {
  val prj      = UInt(RegConfig.PHYS_REG_BITS.W)
  val jIsArf   = Bool()
  val dataj    = UInt(32.W)
  val prk      = UInt(RegConfig.PHYS_REG_BITS.W)
  val kIsArf   = Bool()
  val datak    = UInt(32.W)
  val preg     = UInt(RegConfig.PHYS_REG_BITS.W)
  // val old_preg = UInt(RegConfig.PHYS_REG_BITS.W)
  val checkpoint = new Bundle {
    val valid = Bool()
    val id    = UInt(RegConfig.CHECKPOINT_DEPTH.W)
  }
  val robIdx   = UInt(RobConfig.ROB_INDEX_WIDTH.W) // ROB索引
  val other = new PipelineConnectIO
}


class aliasTableEntry extends Bundle {
  val inARF      = Bool() // if true, read arf; else read rob(robPointer)
  val preg       = UInt(RegConfig.PHYS_REG_BITS.W)
  val neverUsed  = Bool() // 是否从未使用过
}

class RegRenaming extends Module {
  val io = IO(new Bundle {
    val in           = Flipped(Decoupled(Vec(4, new RenameInput)))
    val out          = Decoupled(Vec(4, new RenameOutput))
    val preg         = Flipped(Decoupled(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W))))
    val rob          = Input(new RobCommit)
    val brMispredict = Input(new BrMisPredInfo)
    val robAllocate  = new RobAllocateIO
    val arf = Output(Vec(32, UInt(32.W))) // 逻辑寄存器
    val flush = Input(Bool())
    // val rollbackChkpt = Input(UInt(RegConfig.CHECKPOINT_DEPTH.W)) // 回滚检查点ID
  })

  val arf = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  dontTouch(arf)
  io.arf := arf
  // 寄存器别名表
  // val rat = RegInit(VecInit.tabulate(RegConfig.ARCH_REG_NUM)(i => 
  //   (i + RegConfig.ARCH_REG_NUM).U(RegConfig.PHYS_REG_BITS.W)))
  val rat = RegInit(VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
    val entry = Wire(new aliasTableEntry)
    entry.inARF := true.B
    entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
    entry.neverUsed := true.B
    entry
  }))

  // val arat = RegInit(VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
  //   val entry = Wire(new aliasTableEntry)
  //   entry.inARF := true.B
  //   entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
  //   entry.neverUsed := true.B
  //   entry
  // }))

  var emptyRat = RegInit(VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
    val entry = Wire(new aliasTableEntry)
    entry.inARF := true.B
    entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
    entry.neverUsed := true.B
    entry
  }))

  // val pregUsing = RegInit(VecInit(Seq.fill(RegConfig.PHYS_REG_NUM + 1)(false.B)))
  // // Define a Vec type for the checkpoint RAT
  // val checkpointRATEntryType = Vec(RegConfig.ARCH_REG_NUM, new aliasTableEntry)
  // val checkpointRAT = RegInit(VecInit(Seq.fill(RegConfig.CHECKPOINT_DEPTH) {
  //   VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
  //     val entry = Wire(new aliasTableEntry)
  //     entry.inARF := true.B
  //     entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
  //     entry.robPointer := 0.U(RobConfig.ROB_INDEX_WIDTH.W)
  //     entry
  //   })
  // }))

  val prf = RegInit(VecInit(Seq.fill(RegConfig.PHYS_REG_NUM + 1)(0.U(32.W))))

  // 准备发送到ROB的分配信息
  for (i <- 0 until 4) {
    val entry = io.robAllocate.allocEntries(i)
    val input = io.in.bits(i)
    val rd = input.ctrl.rfDest
    val rfWen = input.ctrl.rfWen
    
    entry := DontCare
    entry.pc := input.pc
    entry.instr := input.instr
    entry.rd := rd
    entry.rfWen := rfWen
    entry.isBranch := input.isBranch
    entry.isStore := (input.ctrl.fuType === FuType.lsu && (LSUOpType.isStore(input.ctrl.fuOpType) || LSUOpType.isSC(input.ctrl.fuOpType)))
    entry.optype := input.ctrl.fuOpType
    entry.fuType := input.ctrl.fuType
    entry.inst_valid := input.inst_valid
    entry.csrOp := input.ctrl.csrOp
    entry.csrNum := input.ctrl.csrNum
    entry.cacopOp := input.ctrl.cacopOp
    entry.cType := input.ctrl.cType
    entry.isCall := input.ctrl.isCall
    entry.isReturn := input.ctrl.isReturn
    entry.checkpoint.valid := input.checkpoint.needSave
    entry.checkpoint.id := input.checkpoint.id % RegConfig.CHECKPOINT_DEPTH.U
  }

  // 连接到ROB分配接口
  io.robAllocate.allocReq := io.in.valid && io.out.fire
  io.robAllocate.allocCount := PopCount(io.in.bits.map(x => io.in.valid && x.inst_valid))
  
  // 握手信号控制
  val canAlloc = io.robAllocate.canAllocate

  val hasPending = RegInit(false.B)
  when(io.flush) {
    hasPending := false.B
  }.elsewhen(io.in.valid && !io.out.fire) {
    hasPending := true.B
  }.elsewhen(io.out.fire) {
    hasPending := false.B
  }

  io.in.ready := canAlloc && io.out.ready && (!hasPending || io.out.fire) && !(RegNext(io.out.fire) && io.in.valid && !io.out.fire)
  io.preg.ready := canAlloc && io.out.ready && (!hasPending || io.out.fire) && !(RegNext(io.out.fire) && io.in.valid && !io.out.fire)
  // dontTouch(canAlloc)
  io.out.valid := io.in.valid && canAlloc

  val temp_prj = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
  val temp_prk = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
  val temp_jIsArf = Wire(Vec(4, Bool()))
  val temp_kIsArf = Wire(Vec(4, Bool()))

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
    val preg = io.preg.bits(i)

    // 目标寄存器分配
    val needAlloc = !rfWen && !isZeroReg && instValid
    io.out.bits(i).preg := Mux(instValid, preg, 0.U)

    // 更新ROB条目中的物理寄存器信息
    io.robAllocate.allocEntries(i).preg := preg
    io.robAllocate.allocEntries(i).old_preg := Mux(rfWen === RfWen.y, rat(rd).preg, 0.U)
    // FIXME:
    io.robAllocate.allocEntries(i).valid := needAlloc && io.out.fire
    io.robAllocate.allocEntries(i).use_preg := needAlloc && io.out.fire

    // 更新RAT
    when(io.in.valid && needAlloc && io.in.valid && io.in.bits(i).inst_valid && io.out.fire) {
      rat(rd).inARF := false.B
      rat(rd).preg := preg
      rat(rd).neverUsed := false.B
    }
    renameFired(i) := io.in.valid && needAlloc && io.in.valid && io.in.bits(i).inst_valid && io.out.fire

    // 源寄存器映射
    val commit = io.rob.commit
    val rjCommit = commit.map { case entry: Valid[rtrBundle] => entry.bits.dest === rj && entry.valid && entry.bits.preg.orR }.reduce(_ || _)
    val rkCommit = commit.map { case entry: Valid[rtrBundle] => entry.bits.dest === rk && entry.valid && entry.bits.preg.orR }.reduce(_ || _)

    temp_prj(i) := Mux(rj.orR, rat(rj).preg, 0.U)
    temp_jIsArf(i) := rat(rj).inARF

    temp_prk(i) := Mux((input.ctrl.src2Type === SrcType.reg || input.ctrl.srcIsRd === SrcIsRd.y) && rk.orR,
                            rat(rk).preg, 
                            0.U)
    temp_kIsArf(i) := rat(rk).inARF
    
    // 立即数不需要物理寄存器
    when(input.ctrl.src2Type === SrcType.imm && input.ctrl.srcIsRd === SrcIsRd.n) {
      temp_prk(i) := 0.U 
    }

    // 检查点处理
    io.out.bits(i).checkpoint.valid := input.checkpoint.needSave
    io.out.bits(i).checkpoint.id := input.checkpoint.id % RegConfig.CHECKPOINT_DEPTH.U
    
    // 分配ROB索引
    io.out.bits(i).robIdx := io.robAllocate.allocResp(i)
  }

  // 组内相关性处理
  io.out.bits(0).prj := temp_prj(0)
  io.out.bits(0).jIsArf := temp_jIsArf(0)
  io.out.bits(0).prk := temp_prk(0)
  io.out.bits(0).kIsArf := temp_kIsArf(0)
  // io.out.bits(0).old_preg := temp_old_preg(0)

  for (i <- 0 until 4) {
    val input = io.in.bits(i)
    val rj = input.ctrl.rfSrc1
    val rk = input.ctrl.rfSrc2
    val rd = input.ctrl.rfDest
    val preg = io.preg.bits

    io.out.bits(i).prj := temp_prj(i)
    io.out.bits(i).prk := temp_prk(i)
    io.out.bits(i).jIsArf := temp_jIsArf(i)
    io.out.bits(i).kIsArf := temp_kIsArf(i)
    // io.out.bits(i).old_preg := temp_old_preg(i)

    for (j <- 0 until i) {
      when(rj.orR && rj === io.in.bits(j).ctrl.rfDest && io.in.bits(j).inst_valid && io.in.bits(j).ctrl.rfWen === RfWen.y) {
        io.out.bits(i).prj := preg(j)
        io.out.bits(i).jIsArf := false.B
      }
      when(rk.orR && rk === io.in.bits(j).ctrl.rfDest && io.in.bits(j).inst_valid && io.in.bits(j).ctrl.rfWen === RfWen.y) {
        io.out.bits(i).prk := preg(j)
        io.out.bits(i).kIsArf := false.B
      }
      // when(rd.orR && rd === io.in.bits(j).ctrl.rfDest && io.in.bits(j).inst_valid && io.in.bits(j).ctrl.rfWen === RfWen.y) {
      //   io.out.bits(i).old_preg := allocated_preg(j)
      // }
    }
    io.out.bits(i).dataj := Mux(rat(io.out.bits(i).prj).neverUsed, arf(rj), prf(io.out.bits(i).prj))
    io.out.bits(i).datak := Mux(rat(io.out.bits(i).prk).neverUsed, arf(rk), prf(io.out.bits(i).prk))
  }

  io.out.bits(0).other := io.in.bits(0).other
  io.out.bits(1).other := io.in.bits(1).other
  io.out.bits(2).other := io.in.bits(2).other
  io.out.bits(3).other := io.in.bits(3).other

  val commit = io.rob.commit

  // 分支预测错误/异常回滚
  when(io.flush) {
    rat := emptyRat
  }

  // retire 
  for(i <- 0 until RobConfig.ROB_CMT_NUM) {
    when(io.rob.commit(i).valid && io.rob.commit(i).bits.inst_valid) {
      // write arf 
      arf(io.rob.commit(i).bits.dest) := io.rob.commit(i).bits.data
      prf(io.rob.commit(i).bits.preg) := io.rob.commit(i).bits.data
      // rat update
      // FIXME: 采用io.brMispredict.brMisPred.valid这么简单粗暴的方式大概率有错误
      when (io.rob.commit(i).bits.preg === rat(io.rob.commit(i).bits.dest).preg && 
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(0).rd && renameFired(0)) &&
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(1).rd && renameFired(1)) &&
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(2).rd && renameFired(2)) &&
            !(io.rob.commit(i).bits.dest === io.robAllocate.allocEntries(3).rd && renameFired(3))) {
        rat(io.rob.commit(i).bits.dest).inARF := true.B
        rat(io.rob.commit(i).bits.dest).neverUsed := false.B
      }
    }
  }
}

class RegRenaming1 extends Module {
  val io = IO(new Bundle {
    val in           = Flipped(Decoupled(Vec(4, new RenameInput)))
    val out          = Decoupled(Vec(4, new RenameInput))
    val preg         = Decoupled(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))
    val rob          = Input(new RobCommit)
    val flush        = Input(Bool())
  })

  val arat = RegInit(VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
    val entry = Wire(new aliasTableEntry)
    entry.inARF := true.B
    entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
    entry.neverUsed := true.B
    entry
  }))

  val validLatch = RegInit(false.B)
  when(io.in.valid && !io.out.fire) {
    validLatch := true.B
  }.elsewhen(io.out.fire) {
    validLatch := false.B
  }
  when(io.flush) {
    validLatch := false.B
  }


  val freeList = Module(new FreeList)

  val allocated_preg = Wire(Vec(4, UInt(RegConfig.PHYS_REG_BITS.W)))

  val canAlloc = freeList.io.count >= 4.U && io.out.ready

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
    freeList.io.allocResp(i).ready := needAlloc && io.in.valid && io.out.fire
    freeList.io.fire := io.out.fire
    
    allocated_preg(i) := Mux(needAlloc && freeList.io.allocResp(i).valid,
                            freeList.io.allocResp(i).bits,
                            0.U)
    io.preg.bits(i) := allocated_preg(i)
  }

  val commit = io.rob.commit

  freeList.io.free(0).valid := commit(0).valid && commit(0).bits.inst_valid && arat(commit(0).bits.dest).preg.orR
  freeList.io.free(0).bits := arat(commit(0).bits.dest).preg

  freeList.io.free(1).valid := commit(1).valid && commit(1).bits.inst_valid && arat(commit(1).bits.dest).preg.orR
  freeList.io.free(1).bits := Mux(commit(0).valid && commit(0).bits.dest === commit(1).bits.dest && commit(0).bits.inst_valid, 
                                  commit(0).bits.preg, 
                                  arat(commit(1).bits.dest).preg)

  // 分支预测错误/异常回滚
  when(io.flush) {
    freeList.io.rollback.bits.head := 1.U
    freeList.io.rollback.bits.tail := 1.U
    freeList.io.rollback.valid := true.B
  }.otherwise {
    freeList.io.rollback.bits := DontCare
    freeList.io.rollback.valid := false.B
  }

  for (i <- 0 until RobConfig.ROB_CMT_NUM) {
    when(io.rob.commit(i).valid && io.rob.commit(i).bits.inst_valid) {
      // 释放物理寄存器
      when(io.rob.commit(i).bits.preg.orR) {
        // pregUsing(io.rob.commit(i).bits.preg) := true.B
        arat(io.rob.commit(i).bits.dest).preg := io.rob.commit(i).bits.preg
        arat(io.rob.commit(i).bits.dest).neverUsed := false.B
        // debugPregUsing(io.rob.commit(i).bits.preg) := true.B
      }
      when(arat(io.rob.commit(i).bits.dest).preg.orR) {
        // debugPregUsing(arat(io.rob.commit(i).bits.dest).preg) := false.B
      }
    }
  }

  var emptyRat = RegInit(VecInit(Seq.fill(RegConfig.ARCH_REG_NUM) {
    val entry = Wire(new aliasTableEntry)
    entry.inARF := true.B
    entry.preg := 0.U(RegConfig.PHYS_REG_BITS.W)
    entry.neverUsed := true.B
    entry
  }))


  when(io.flush) {
    arat := emptyRat
  }

  io.out.bits := io.in.bits

  io.in.ready := canAlloc && io.out.ready

  io.out.valid := (io.in.valid || validLatch) && canAlloc && 
                  PopCount(freeList.io.allocResp.map(_.valid)) === PopCount(freeList.io.allocReq.map(_.valid))
  io.preg.valid := io.out.valid
}