package core
import chisel3._
import IssueConfig._

class OrderIssueQueue(val SIZE: Int = 8, val MAX_CNT: Int = 4) extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new dispatch_out_info))
    val out = Decoupled(Output(new PipelineConnectIO))
    val inst_cnt = Input(UInt(3.W))
    val allReady = Input(Bool())
    // val from_ready = Output(Bool()) io.in.ready
    // val from_valid = Input(Bool()) io.in.valid
    // val to_valid = Output(Bool()) io.out.valid
    // val to_ready = Input(Bool()) io.out.ready

    val busyreg = Input(Vec(PHYS_REG_NUM, Bool()))
    val pram_read = Flipped(new payloadram_read_info)
    val flush = Input(Bool())
  })

  val mem = Reg(Vec(SIZE.toInt, new PipelineConnectIO))
  val valid_vec = RegInit(VecInit(Seq.fill(SIZE.toInt)(false.B)))
  val write_ptr = RegInit(0.U(log2Ceil(SIZE).W))
  val read_ptr = RegInit(0.U(log2Ceil(SIZE).W))
  val valid_count = RegInit(0.U((log2Ceil(SIZE.toInt) + 1).W))
  val enq_count = Wire(UInt(3.W))
  val deq_count = Mux(io.out.fire, 1.U, 0.U)

  // FIXME: ???
  val inFire = io.in.valid && io.allReady
  val valid_count_wire = PopCount(valid_vec)
  val next_count = valid_count_wire +& Mux(inFire, enq_count, 0.U) - deq_count
  io.in.ready := RegNext(next_count) + io.inst_cnt <= (SIZE).U
  dontTouch(next_count)
  
  enq_count := io.in.bits.inst_cnt
  // valid_count := Mux(io.flush, 0.U, valid_count + enq_count - deq_count)
  valid_count := PopCount(valid_vec)

  // read
  val debug_read_ptr = WireInit(read_ptr)
  val debug_read_ptr_valid = WireInit(valid_vec(read_ptr))
  val debug_read_ptr_prj = WireInit(mem(read_ptr).prj)
  val debug_read_ptr_prk = WireInit(mem(read_ptr).prk)
  dontTouch(debug_read_ptr)
  dontTouch(debug_read_ptr_valid)
  dontTouch(debug_read_ptr_prj)
  dontTouch(debug_read_ptr_prk)

  val can_issue = (!io.busyreg(mem(read_ptr).prj) || mem(read_ptr).jIsArf) && (!io.busyreg(mem(read_ptr).prk) || mem(read_ptr).kIsArf) && valid_vec(read_ptr)
  //io.out := mem(read_ptr)
  io.pram_read.src1 := mem(read_ptr).prj
  io.pram_read.src2 := mem(read_ptr).prk
  val out = mem(read_ptr)
  val prj_0 = Fill(32, mem(read_ptr).prj =/= 0.U)
  val prk_0 = Fill(32, mem(read_ptr).prk =/= 0.U)
  io.out.bits := out
  io.out.bits.src1 := Mux(io.out.bits.jIsArf, io.out.bits.dataj, prj_0 & io.pram_read.pram_data1)
  io.out.bits.src2 := Mux(io.out.bits.kIsArf, io.out.bits.datak, prk_0 & io.pram_read.pram_data2)
  // out.src1 := io.pram_read.pram_data1
  // out.src2 := io.pram_read.pram_data2
  io.out.valid := can_issue
  when(io.out.fire) {  // 发生握手才读出
    valid_vec(read_ptr) := false.B
    read_ptr := (read_ptr + 1.U) % SIZE.U
  }

  // write
  switch(io.in.bits.inst_cnt) {
    is(1.U) {
      when(inFire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        write_ptr := (write_ptr + 1.U) % SIZE.U
      }
    }
    is(2.U) {
      when(inFire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem((write_ptr + 1.U) % SIZE.U) := io.in.bits.inst_vec(1)
        valid_vec((write_ptr + 1.U) % SIZE.U) := true.B
        write_ptr := (write_ptr + 2.U) % SIZE.U
      }
    }
    is(3.U) {
      when(inFire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem((write_ptr + 1.U) % SIZE.U) := io.in.bits.inst_vec(1)
        valid_vec((write_ptr + 1.U) % SIZE.U) := true.B
        mem((write_ptr + 2.U) % SIZE.U) := io.in.bits.inst_vec(2)
        valid_vec((write_ptr + 2.U) % SIZE.U) := true.B
        write_ptr := (write_ptr + 3.U) % SIZE.U
      }
    }
    is(4.U) {
      when(inFire) {
        mem(write_ptr) := io.in.bits.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem((write_ptr + 1.U) % SIZE.U) := io.in.bits.inst_vec(1)
        valid_vec((write_ptr + 1.U) % SIZE.U) := true.B
        mem((write_ptr + 2.U) % SIZE.U) := io.in.bits.inst_vec(2)
        valid_vec((write_ptr + 2.U) % SIZE.U) := true.B
        mem((write_ptr + 3.U) % SIZE.U) := io.in.bits.inst_vec(3)
        valid_vec((write_ptr + 3.U) % SIZE.U) := true.B
        write_ptr := (write_ptr + 4.U) % SIZE.U
      }
    }
  }

  // flush
  when(io.flush) {
    write_ptr := 0.U
    read_ptr := 0.U
    for (i <- 0 until SIZE.toInt) {
      valid_vec(i) := false.B
    }
  }
}