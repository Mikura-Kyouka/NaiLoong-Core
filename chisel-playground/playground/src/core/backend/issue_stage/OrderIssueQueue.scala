import chisel3._
import IssueConfig._

class OrderIssueQueue extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in = Input(new dispatch_out_info)
    val out = Output(new inst_info)
    val from_ready = Output(Bool())
    val from_valid = Input(Bool())
    val to_valid = Output(Bool())
    val to_ready = Input(Bool())

    val busyreg = Input(Vec(PHYS_REG_NUM, Bool()))
    val retire_inst = Input(Valid(new retire_inst_info))
  })

  val mem = Reg(Vec(QUEUE_SIZE.toInt, new inst_info))
  val valid_vec = RegInit(VecInit(Seq.fill(QUEUE_SIZE.toInt)(false.B)))
  val write_ptr = RegInit(0.U(log2Ceil(QUEUE_SIZE).W))
  val read_ptr = RegInit(0.U(log2Ceil(QUEUE_SIZE).W))

  val can_accept = (write_ptr - read_ptr) >= io.in.inst_cnt
  io.from_ready := can_accept

  // write
  switch(io.in.inst_cnt) {
    is(1.U) {
      when(io.from_valid && io.from_ready) {
        mem(write_ptr) := io.in.inst_vec(0)
        valid_vec(write_ptr) := true.B
        write_ptr := write_ptr + 1.U
      }
    }
    is(2.U) {
      when(io.from_valid && io.from_ready) {
        mem(write_ptr) := io.in.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem(write_ptr + 1.U) := io.in.inst_vec(1)
        valid_vec(write_ptr + 1.U) := true.B
        write_ptr := write_ptr + 2.U
      }
    }
    is(3.U) {
      when(io.from_valid && io.from_ready) {
        mem(write_ptr) := io.in.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem(write_ptr + 1.U) := io.in.inst_vec(1)
        valid_vec(write_ptr + 1.U) := true.B
        mem(write_ptr + 2.U) := io.in.inst_vec(2)
        valid_vec(write_ptr + 2.U) := true.B
        write_ptr := write_ptr + 3.U
      }
    }
    is(4.U) {
      when(io.from_valid && io.from_ready) {
        mem(write_ptr) := io.in.inst_vec(0)
        valid_vec(write_ptr) := true.B
        mem(write_ptr + 1.U) := io.in.inst_vec(1)
        valid_vec(write_ptr + 1.U) := true.B
        mem(write_ptr + 2.U) := io.in.inst_vec(2)
        valid_vec(write_ptr + 2.U) := true.B
        mem(write_ptr + 3.U) := io.in.inst_vec(3)
        valid_vec(write_ptr + 3.U) := true.B
        write_ptr := write_ptr + 4.U
      }
    }
  }

  // read
  val can_issue = !io.busyreg(mem(read_ptr).preg0) && !io.busyreg(mem(read_ptr).preg1) && valid_vec(read_ptr)
  io.out := mem(read_ptr)
  io.to_valid := can_issue
  when(io.to_valid && io.to_ready) {  // 发生握手才读出
    valid_vec(read_ptr) := false.B
    read_ptr := read_ptr + 1.U
  }

  // retire
  when(io.retire_inst.valid) {
    for(i <- 0 until QUEUE_SIZE.toInt) {
      when(mem(i).preg0 === io.retire_inst.bits.preg) {
        mem(i).src1_is_areg := true.B
      }
      when(mem(i).preg1 === io.retire_inst.bits.preg) {
        mem(i).src2_is_areg := true.B
      }
    }
  }
}

object GenO extends App {
    val firtoolOptions = Array(
      "--lowering-options=" + List(
        // make yosys happy
        // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
        "disallowLocalVariables",
        "disallowPackedArrays",
        "locationInfoStyle=wrapInAtSquareBracket",
        "mitigateVivadoArrayIndexConstPropBug"
      ).reduce(_ + "," + _)
    )
    circt.stage.ChiselStage.emitSystemVerilogFile(new OrderIssueQueue(), args, firtoolOptions)
}