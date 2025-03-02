import chisel3._
import IssueConfig._

class UnorderIssueQueue extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in = Input(new dispatch_out_info) // 从 Dispatch 模块传入的指令
    val out = Output(new inst_info) // 从队列中取出的指令
    val from_ready = Output(Bool())  // 发射队列没满为真
    val from_valid = Input(Bool())  
    val to_valid = Output(Bool())  // 发射队列不为空为真
    val to_ready = Input(Bool())

    val busyreg = Input(Vec(PHYS_REG_NUM, Bool()))  // 物理寄存器是否被占用
    val retire_inst = Input(Valid(new retire_inst_info))
  })

  val mem = Reg(Vec(QUEUE_SIZE.toInt, new inst_info))
  val valid_vec = RegInit(VecInit(Seq.fill(QUEUE_SIZE.toInt)(false.B)))
  val valid_count= RegInit(0.U(log2Ceil(QUEUE_SIZE.toInt).W))

  // 向发射队列写入指令
  val can_accept = (valid_count + io.in.inst_cnt) <= QUEUE_SIZE.asUInt
  io.from_ready := can_accept
  switch(io.in.inst_cnt) {
    is(1.U) {
      when(io.from_valid && io.from_ready) {
        mem(valid_count) := io.in.inst_vec(0)
        valid_vec(valid_count) := true.B
        valid_count := valid_count + 1.U
      }
    }
    is(2.U) {
      when(io.from_valid && io.from_ready) {
        mem(valid_count) := io.in.inst_vec(0)
        valid_vec(valid_count) := true.B
        mem(valid_count + 1.U) := io.in.inst_vec(1)
        valid_vec(valid_count + 1.U) := true.B
        valid_count := valid_count + 2.U
      }
    }
  }

  // 判断指令是否可以发射
  val can_issue_vec = Wire(Vec(QUEUE_SIZE, Bool()))
  for(i <- 0 until QUEUE_SIZE) {
    can_issue_vec(i) := !io.busyreg(mem(i).preg0) && !io.busyreg(mem(i).preg1) && valid_vec(i)
  }
  val can_issue = can_issue_vec.reduce(_ || _)
  io.to_valid := can_issue

  // 发射并压缩队列
  val first_can_issue_index = PriorityEncoder(can_issue_vec)
  io.out := mem(first_can_issue_index)
  when(io.to_valid && io.to_ready) {
    for(i <- 0 until (QUEUE_SIZE - 1)) {
      when(i.U >= first_can_issue_index) {
        mem(i) := mem(i + 1)
        valid_vec(i):= valid_vec(i + 1)
      }
    }
    valid_vec(QUEUE_SIZE - 1) := false.B
    valid_count := valid_count - 1.U
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

object GenU extends App {
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
    circt.stage.ChiselStage.emitSystemVerilogFile(new UnorderIssueQueue(), args, firtoolOptions)
}