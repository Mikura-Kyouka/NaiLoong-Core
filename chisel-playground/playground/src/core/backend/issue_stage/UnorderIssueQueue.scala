package core

import chisel3._
import IssueConfig._

class UnorderIssueQueue(val check_dest: Boolean = false) extends Module {
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

  val mem = RegInit(VecInit(Seq.fill(QUEUE_SIZE)(0.U.asTypeOf(new PipelineConnectIO))))
  val valid_vec = RegInit(VecInit(Seq.fill(QUEUE_SIZE.toInt)(false.B)))
  val valid_count= RegInit(0.U(log2Ceil(QUEUE_SIZE.toInt).W))

  // 向发射队列写入指令
  val can_accept = valid_count === 0.U || (valid_count + io.in.bits.inst_cnt) <= QUEUE_SIZE.asUInt
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

  // 发射并压缩队列
  val first_can_issue_index = PriorityEncoder(can_issue_vec)
  //io.out := mem(first_can_issue_index)
  io.pram_read.src1 := mem(first_can_issue_index).prj
  io.pram_read.src2 := mem(first_can_issue_index).prk
  val out = mem(first_can_issue_index)
  // FIXME: src comes from arf/payloadram
  io.out.bits := out
  io.out.bits.src1 := Mux(io.out.bits.jIsArf, io.out.bits.dataj, io.pram_read.pram_data1)
  io.out.bits.src2 := Mux(io.out.bits.kIsArf, io.out.bits.datak, io.pram_read.pram_data2)
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

  // flush
  when(io.flush) {
    valid_count := 0.U
    for (i <- 0 until QUEUE_SIZE.toInt) {
      valid_vec(i) := false.B
    }
  }
}