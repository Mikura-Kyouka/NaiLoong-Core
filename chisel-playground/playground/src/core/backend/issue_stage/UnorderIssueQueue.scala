package core

import chisel3._
import IssueConfig._

class UnorderIssueQueue extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new dispatch_out_info)) // 从 Dispatch 模块传入的指令
    val out = Decoupled(Output(new inst_info)) // 从队列中取出的指令
    // val from_ready = Output(Bool())  // 发射队列没满为真
    // val from_valid = Input(Bool())  
    // val to_valid = Output(Bool())  // 发射队列不为空为真
    // val to_ready = Input(Bool())

    val busyreg = Input(Vec(PHYS_REG_NUM, Bool()))  // 物理寄存器是否被占用
    val pram_read = Flipped(new payloadram_read_info)  // 读取 payload ram
  })

  val mem = Reg(Vec(QUEUE_SIZE.toInt, new inst_info))
  val valid_vec = RegInit(VecInit(Seq.fill(QUEUE_SIZE.toInt)(false.B)))
  val valid_count= RegInit(0.U(log2Ceil(QUEUE_SIZE.toInt).W))

  // 向发射队列写入指令
  val can_accept = (valid_count + io.in.bits.inst_cnt) <= QUEUE_SIZE.asUInt
  io.in.ready := can_accept
  switch(io.in.bits.inst_cnt) {
    is(1.U) {
      when(io.in.valid && io.in.ready) {
        mem(valid_count) := io.in.bits.inst_vec(0)
        valid_vec(valid_count) := true.B
        valid_count := valid_count + 1.U
      }
    }
    is(2.U) {
      when(io.in.valid && io.in.ready) {
        mem(valid_count) := io.in.bits.inst_vec(0)
        valid_vec(valid_count) := true.B
        mem(valid_count + 1.U) := io.in.bits.inst_vec(1)
        valid_vec(valid_count + 1.U) := true.B
        valid_count := valid_count + 2.U
      }
    }
  }

  // 判断指令是否可以发射
  val can_issue_vec = Wire(Vec(QUEUE_SIZE, Bool()))
  for(i <- 0 until QUEUE_SIZE) {
    can_issue_vec(i) := !io.busyreg(mem(i).preg1) && !io.busyreg(mem(i).preg2) && valid_vec(i)
  }
  val can_issue = can_issue_vec.reduce(_ || _)
  io.out.valid := can_issue

  // 发射并压缩队列
  val first_can_issue_index = PriorityEncoder(can_issue_vec)
  //io.out := mem(first_can_issue_index)
  io.pram_read.src1 := mem(first_can_issue_index).preg1
  io.pram_read.src2 := mem(first_can_issue_index).preg2
  val out = mem(first_can_issue_index)
  out.data1 := io.pram_read.pram_data1
  out.data2 := io.pram_read.pram_data2
  io.out.bits := out
  when(io.out.valid) {
    for(i <- 0 until (QUEUE_SIZE - 1)) {
      when(i.U >= first_can_issue_index) {
        mem(i) := mem(i + 1)
        valid_vec(i):= valid_vec(i + 1)
      }
    }
    valid_vec(QUEUE_SIZE - 1) := false.B
    valid_count := valid_count - 1.U
  }
}