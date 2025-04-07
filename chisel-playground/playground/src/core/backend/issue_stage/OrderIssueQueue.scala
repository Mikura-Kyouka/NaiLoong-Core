package core
import chisel3._
import IssueConfig._

class OrderIssueQueue extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new dispatch_out_info))
    val out = Decoupled(Output(new PipelineConnectIO))
    // val from_ready = Output(Bool()) io.in.ready
    // val from_valid = Input(Bool()) io.in.valid
    // val to_valid = Output(Bool()) io.out.valid
    // val to_ready = Input(Bool()) io.out.ready

    val busyreg = Input(Vec(PHYS_REG_NUM, Bool()))
    val pram_read = Flipped(new payloadram_read_info)
  })

  val mem = Reg(Vec(QUEUE_SIZE.toInt, new PipelineConnectIO))
  val valid_vec = RegInit(VecInit(Seq.fill(QUEUE_SIZE.toInt)(false.B)))
  val write_ptr = RegInit(0.U(log2Ceil(QUEUE_SIZE).W))
  val read_ptr = RegInit(0.U(log2Ceil(QUEUE_SIZE).W))

  val can_accept = (write_ptr - read_ptr) >= io.in.bits.inst_cnt
  io.in.ready := can_accept

  // write
  switch(io.in.bits.inst_cnt) {
    is(1.U) {
      when(io.in.fire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        write_ptr := write_ptr + 1.U
      }
    }
    is(2.U) {
      when(io.in.fire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem(write_ptr + 1.U) := io.in.bits.inst_vec(1)
        valid_vec(write_ptr + 1.U) := true.B
        write_ptr := write_ptr + 2.U
      }
    }
    is(3.U) {
      when(io.in.fire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem(write_ptr + 1.U) := io.in.bits.inst_vec(1)
        valid_vec(write_ptr + 1.U) := true.B
        mem(write_ptr + 2.U) := io.in.bits.inst_vec(2)
        valid_vec(write_ptr + 2.U) := true.B
        write_ptr := write_ptr + 3.U
      }
    }
    is(4.U) {
      when(io.in.fire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem(write_ptr + 1.U) := io.in.bits.inst_vec(1)
        valid_vec(write_ptr + 1.U) := true.B
        mem(write_ptr + 2.U) := io.in.bits.inst_vec(2)
        valid_vec(write_ptr + 2.U) := true.B
        mem(write_ptr + 3.U) := io.in.bits.inst_vec(3)
        valid_vec(write_ptr + 3.U) := true.B
        write_ptr := write_ptr + 4.U
      }
    }
  }

  // read
  val can_issue = !io.busyreg(mem(read_ptr).prj) && !io.busyreg(mem(read_ptr).prk) && valid_vec(read_ptr)
  //io.out := mem(read_ptr)
  io.pram_read.src1 := mem(read_ptr).prj
  io.pram_read.src2 := mem(read_ptr).prk
  val out = mem(read_ptr)
  out.src1 := io.pram_read.pram_data1
  out.src2 := io.pram_read.pram_data2
  io.out.bits := out
  io.out.valid := can_issue
  when(io.out.fire) {  // 发生握手才读出
    valid_vec(read_ptr) := false.B
    read_ptr := read_ptr + 1.U
  }
}