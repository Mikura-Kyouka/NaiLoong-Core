import chisel3._
import IssueConfig._

class Dispatch extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(ISSUE_WIDTH, new dispatch_in_info))
    val out = Output(Vec(ISSUE_WIDTH, new dispatch_out_info))
  })

   for (q <- io.out) {
      q.inst_cnt := 0.U
      for(i <- 0 until ISSUE_WIDTH) {
        q.inst_vec(i) := DontCare
      }
    }

  for(i <- 0 until ISSUE_WIDTH) {
    val inst = io.in(i)

    val alu_cnt_before = (0 until i).map { j =>
      Mux(io.in(j).op === 0.U, 1.U(2.W), 0.U(2.W))
    }.reduceOption(_ + _).getOrElse(0.U(2.W))
    
    val muldiv_cnt_before = (0 until i).map { j =>
      Mux(io.in(j).op === 1.U, 1.U(2.W), 0.U(2.W))
    }.reduceOption(_ + _).getOrElse(0.U(2.W))
    
    val loadstore_cnt_before = (0 until i).map { j =>
      Mux(io.in(j).op === 2.U, 1.U(2.W), 0.U(2.W))
    }.reduceOption(_ + _).getOrElse(0.U(2.W))

    // 根据指令类型分发
    when(inst.op === 0.U) { // ALU 指令
      when((alu_cnt_before % 2.U) === 0.U) {
        io.out(0).inst_vec(alu_cnt_before >> 1) := inst
        io.out(0).inst_cnt := (alu_cnt_before >> 1) + 1.U
      } .otherwise {
        io.out(1).inst_vec(alu_cnt_before >> 1) := inst
        io.out(1).inst_cnt := (alu_cnt_before >> 1) + 1.U
      }
    } .elsewhen(inst.op === 1.U) { // MUL/DIV 指令
      io.out(2).inst_vec(muldiv_cnt_before) := inst
      io.out(2).inst_cnt := muldiv_cnt_before + 1.U
    } .elsewhen(inst.op === 2.U) { // Load/Store 指令
      io.out(3).inst_vec(loadstore_cnt_before) := inst
      io.out(3).inst_cnt := loadstore_cnt_before + 1.U
    }
  }
}

object GenDispatch extends App {
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
    circt.stage.ChiselStage.emitSystemVerilogFile(new Dispatch(), args, firtoolOptions)
}