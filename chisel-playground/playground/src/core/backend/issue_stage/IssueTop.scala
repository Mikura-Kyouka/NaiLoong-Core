// package core
// import chisel3._
// import IssueConfig._
// import chisel3.stage._

// // arith issue queue*2, mul/div issue queue*1, load/store issue queue*1
// class IssueTop extends Bundle {
//   val io = IO(new Bundle {
//     val from = Flipped(Decoupled(Vec(4, new PipelineConnectIO)))
//     val cmtInstr = Flipped(Decoupled(new commit_inst_info))
//     val rtrInstr = Flipped(Decoupled(new retire_inst_info))
//     val to = Decoupled(Vec(4, new PipelineConnectIO))
//   }) 
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