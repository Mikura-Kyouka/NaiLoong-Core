// package core
// import chisel3._
// import IssueConfig._
// import chisel3.stage._

// // arith issue queue*2, mul/div issue queue*1, load/store issue queue*1
// class IssueTop extends Module {
//   import chisel3.util._
//   val io = IO(new Bundle {
//     val from = Vec(ISSUE_WIDTH, Flipped(Decoupled(new renaming_to_issue)))
//     val to = Vec(ISSUE_WIDTH, Decoupled(new issue_to_execute))

//     val commit_inst = Flipped(Valid(new commit_inst_info))
//     val retire_inst = Flipped(Valid(new retire_inst_info))
//   })

//   val dispatch = Module(new Dispatch)
//   dispatch.io.in(0) <> io.from(0).bits
//   dispatch.io.in(1) <> io.from(1).bits
//   dispatch.io.in(2) <> io.from(2).bits
//   dispatch.io.in(3) <> io.from(3).bits

//   // 分发到不同的发射队列
//   val unorder_issue_queue0 = Module(new UnorderIssueQueue)
//   unorder_issue_queue0.io.in <> dispatch.io.out(0)
//   val unorder_issue_queue1 = Module(new UnorderIssueQueue)
//   unorder_issue_queue1.io.in <> dispatch.io.out(1)
//   val order_issue_queue0 = Module(new OrderIssueQueue)
//   order_issue_queue0.io.in <> dispatch.io.out(2)
//   val order_issue_queue1 = Module(new OrderIssueQueue)
//   order_issue_queue1.io.in <> dispatch.io.out(3)

//   // 连接退役指令
//   val busyreg = RegInit(VecInit(Seq.fill(PHYS_REG_NUM)(false.B)))
//   for(i <- 0 until ISSUE_WIDTH) {
//     val dest = io.from(i).bits.dest
//     when(io.from(i).valid && io.from(i).ready) {
//       busyreg(dest) := true.B
//     }
//   }
//   when(io.commit_inst.valid) {
//     busyreg(io.commit_inst.bits.inst.dest) := false.B
//   }

//   // 连接忙信号
//   unorder_issue_queue0.io.busyreg := busyreg
//   unorder_issue_queue1.io.busyreg := busyreg
//   order_issue_queue0.io.busyreg := busyreg
//   order_issue_queue1.io.busyreg := busyreg

//   val payloadram = Module(new PayloadRAM)
//   unorder_issue_queue0.io.pram_read <> payloadram.io.read(0)
//   unorder_issue_queue1.io.pram_read <> payloadram.io.read(1)
//   order_issue_queue0.io.pram_read <> payloadram.io.read(2)
//   order_issue_queue1.io.pram_read <> payloadram.io.read(3)
//   payloadram.io.write.dest := io.retire_inst.bits.preg
//   payloadram.io.write.pram_data := io.retire_inst.bits.data
//   payloadram.io.write.valid := io.retire_inst.valid

//   // 连接输出
//   io.to(0).bits <> unorder_issue_queue0.io.out
//   io.to(1).bits <> unorder_issue_queue1.io.out
//   io.to(2).bits <> order_issue_queue0.io.out
//   io.to(3).bits <> order_issue_queue1.io.out

//   // 连接握手信号
//   io.to(0).valid := unorder_issue_queue0.io.to_valid
//   unorder_issue_queue0.io.to_ready := io.to(0).ready
//   io.to(1).valid := unorder_issue_queue1.io.to_valid
//   unorder_issue_queue1.io.to_ready := io.to(1).ready
//   io.to(2).valid := order_issue_queue0.io.to_valid
//   order_issue_queue0.io.to_ready := io.to(2).ready
//   io.to(3).valid := order_issue_queue1.io.to_valid
//   order_issue_queue1.io.to_ready := io.to(3).ready

//   unorder_issue_queue0.io.from_valid := io.from(0).valid
//   unorder_issue_queue1.io.from_valid := io.from(1).valid
//   order_issue_queue0.io.from_valid := io.from(2).valid
//   order_issue_queue1.io.from_valid := io.from(3).valid
//   io.from(0).ready := unorder_issue_queue0.io.from_ready
//   io.from(1).ready := unorder_issue_queue1.io.from_ready
//   io.from(2).ready := order_issue_queue0.io.from_ready
//   io.from(3).ready := order_issue_queue1.io.from_ready
// }

// object GenIssueTop extends App {
//     val firtoolOptions = Array(
//       "--lowering-options=" + List(
//         // make yosys happy
//         // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
//         "disallowLocalVariables",
//         "disallowPackedArrays",
//         "locationInfoStyle=wrapInAtSquareBracket",
//         "mitigateVivadoArrayIndexConstPropBug"
//       ).reduce(_ + "," + _)
//     )
//     circt.stage.ChiselStage.emitSystemVerilogFile(new IssueTop(), args, firtoolOptions)
// }