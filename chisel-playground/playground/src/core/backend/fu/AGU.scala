package core

import chisel3._
import chisel3.util._

class AGU extends Module {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(Output(new PipelineConnectIO)))
    val out = Decoupled(Output(new PipelineConnectIO))
  })
  io.out.bits := io.in.bits
  io.out.bits.src1 := io.in.bits.src1 + io.in.bits.imm
  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
}