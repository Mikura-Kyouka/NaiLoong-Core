package core

import chisel3._
import IssueConfig._

class UnorderIssueQueue(val check_dest: Boolean = false, val SIZE: Int = 8, val MAX_CNT: Int = 4) extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new dispatch_out_info)) // 从 Dispatch 模块传入的指令
    val out = Decoupled(Output(new PipelineConnectIO)) // 从队列中取出的指令
    // val from_ready = Output(Bool())  // 发射队列没满为真
    // val from_valid = Input(Bool())  
    // val to_valid = Output(Bool())  // 发射队列不为空为真
    // val to_ready = Input(Bool())

    val busyreg = Input(Vec(PHYS_REG_NUM, Bool()))  // 物理寄存器是否被占用
    val pram_read = Flipped(new payloadram_read_info)  // 读取 payload ram
    val flush = Input(Bool())
  })

  val io_raw = IO(new Bundle {
    val dest = Input(UInt(PHYS_REG_BITS.W))
  })

  val mem = RegInit(VecInit(Seq.fill(SIZE)(0.U.asTypeOf(new PipelineConnectIO))))
  val valid_vec = RegInit(VecInit(Seq.fill(SIZE.toInt)(false.B)))
  val valid_count= RegInit(0.U((log2Ceil(SIZE.toInt) + 1).W))
  val next_mem = WireInit(mem)
  val next_valid_vec = WireInit(valid_vec)
  dontTouch(next_valid_vec)

  // 判断是否可以接受
  // val can_accept = valid_count === 0.U || (valid_count + io.in.bits.inst_cnt) <= SIZE.asUInt
  val can_accept = valid_count === 0.U || valid_count <= (SIZE - MAX_CNT).U
  io.in.ready := can_accept

  // 计算下一拍的valid_count
  val enq_count = Wire(UInt(3.W))  // 最多一次入两条指令
  enq_count := 0.U
  when(io.in.valid) {
    enq_count := io.in.bits.inst_cnt
  }
  val deq_count = Mux(io.out.fire, 1.U, 0.U)

  // 判断指令是否可以发射
  val can_issue_vec = Wire(Vec(SIZE, Bool()))
  for(i <- 0 until SIZE) {
    if(check_dest){
      can_issue_vec(i) := !io.busyreg(mem(i).prj) && !io.busyreg(mem(i).prk) && valid_vec(i) &&
                          mem(i).prj =/= io_raw.dest && mem(i).prk =/= io_raw.dest
    }
    else {
      can_issue_vec(i) := (!io.busyreg(mem(i).prj) || mem(i).jIsArf) && (!io.busyreg(mem(i).prk) || mem(i).kIsArf) && valid_vec(i)
    }
  }
  val can_issue = can_issue_vec.reduce(_ || _)
  io.out.valid := can_issue

  // 发射指令
  val first_can_issue_index = PriorityEncoder(can_issue_vec)
  //io.out := mem(first_can_issue_index)
  io.pram_read.src1 := mem(first_can_issue_index).prj
  io.pram_read.src2 := mem(first_can_issue_index).prk
  val out = mem(first_can_issue_index)
  // FIXME: src comes from arf/payloadram
  val prj_0 = Fill(32, mem(first_can_issue_index).prj =/= 0.U)
  val prk_0 = Fill(32, mem(first_can_issue_index).prk =/= 0.U)
  io.out.bits := out
  io.out.bits.src1 := Mux(io.out.bits.ctrl.src1Type === SrcType.pc, io.out.bits.pc, 
                      Mux(io.out.bits.jIsArf, io.out.bits.dataj, prj_0 & io.pram_read.pram_data1))
  io.out.bits.src2 := Mux(io.out.bits.kIsArf, io.out.bits.datak, prk_0 & io.pram_read.pram_data2)

  // 压缩队列
  when(io.out.fire) {
    for (i <- 0 until SIZE - 1) {
      when(i.U >= first_can_issue_index) {
        next_mem(i) := mem(i + 1)
        next_valid_vec(i) := valid_vec(i + 1)
      }
    }
    next_valid_vec(SIZE - 1) := false.B
  }

  // write
  when(io.in.fire) {
    val base = valid_count - deq_count
    for (i <- 1 until MAX_CNT + 1) {
      when(io.in.bits.inst_cnt === i.U) {
        for (j <- 0 until i) {
          next_mem(base + j.U) := io.in.bits.inst_vec(j)
          next_valid_vec(base + j.U) := true.B
        }
      }
    }
    // when(io.in.bits.inst_cnt === 1.U) {
    //   next_mem(base) := io.in.bits.inst_vec(0)
    //   next_valid_vec(base) := true.B
    // }
    // when(io.in.bits.inst_cnt === 2.U) {
    //   next_mem(base) := io.in.bits.inst_vec(0)
    //   next_valid_vec(base) := true.B
    //   next_mem(base + 1.U) := io.in.bits.inst_vec(1)
    //   next_valid_vec(base + 1.U) := true.B
    // }
    // when(io.in.bits.inst_cnt === 3.U) {
    //   next_mem(base) := io.in.bits.inst_vec(0)
    //   next_valid_vec(base) := true.B
    //   next_mem(base + 1.U) := io.in.bits.inst_vec(1)
    //   next_valid_vec(base + 1.U) := true.B
    //   next_mem(base + 2.U) := io.in.bits.inst_vec(2)
    //   next_valid_vec(base + 2.U) := true.B
    // }
    // when(io.in.bits.inst_cnt === 4.U) {
    //   next_mem(base) := io.in.bits.inst_vec(0)
    //   next_valid_vec(base) := true.B
    //   next_mem(base + 1.U) := io.in.bits.inst_vec(1)
    //   next_valid_vec(base + 1.U) := true.B
    //   next_mem(base + 2.U) := io.in.bits.inst_vec(2)
    //   next_valid_vec(base + 2.U) := true.B
    //   next_mem(base + 3.U) := io.in.bits.inst_vec(3)
    //   next_valid_vec(base + 3.U) := true.B
    // }
  }

  // flush
  when(io.flush) {
    for (i <- 0 until SIZE.toInt) {
      next_valid_vec(i) := false.B
      next_mem(i).valid := false.B
    }
  }

  // 更新状态
  mem := next_mem
  valid_vec := next_valid_vec
  valid_count := Mux(io.flush, 0.U, valid_count + enq_count - deq_count)
}