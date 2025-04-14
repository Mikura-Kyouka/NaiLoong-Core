// package core

// import chisel3._
// import chisel3.util._

// import IssueConfig._

// class DispatchTop extends Module {
//   val io = IO(new Bundle {
//     val from = Flipped(Decoupled(Vec(4, new PipelineConnectIO)))
//     val to = Vec(ISSUE_WIDTH, Decoupled(new PipelineConnectIO))
//   })

//   val dispatch = Module(new Dispatch)
//   // connect input
//   io.from.ready := dispatch.io.in(0).ready &&
//                    dispatch.io.in(1).ready && 
//                    dispatch.io.in(2).ready && 
//                    dispatch.io.in(3).ready
//   for(i <- 0 until 4) {
//     dispatch.io.in(i).valid := io.from.valid

//     dispatch.io.in(i).bits.areg1 := io.from.bits(i).ctrl.rfSrc1
//     dispatch.io.in(i).bits.areg2 := io.from.bits(i).ctrl.rfSrc2
//     dispatch.io.in(i).bits.preg1 := io.from.bits(i).prj
//     dispatch.io.in(i).bits.preg2 := io.from.bits(i).prk
//     dispatch.io.in(i).bits.data1 := io.from.bits(i).src1
//     dispatch.io.in(i).bits.data2 := io.from.bits(i).src2
//     dispatch.io.in(i).bits.dest := io.from.bits(i).preg
//     dispatch.io.in(i).bits.op := io.from.bits(i).ctrl

//     dispatch.io.in(i).bits.imm := io.from.bits(i).imm
//     dispatch.io.in(i).bits.src2_is_imm := (io.from.bits(i).ctrl.src2Type === 1.U)
//   }

//   // connect output
//   for(i <- 0 until ISSUE_WIDTH) {
//     io.to(i).valid := dispatch.io.out(i).valid
    
//     io.to(i).bits.
//   }
// }