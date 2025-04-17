package core
import chisel3._
import IssueConfig._
import chisel3.util._
import chisel3.stage._
import utils.PipelineConnect

class Execute extends Module {
  val io = IO(new Bundle {
    val in = Vec(ISSUE_WIDTH, Flipped(Decoupled(new PipelineConnectIO)))
    val out = Vec(ISSUE_WIDTH, Decoupled(new FuOut))
    val fire = Vec(ISSUE_WIDTH, Output(Bool()))
    val lsAXI = new AXI
    val robCommit = Input(new RobCommit)
  })

  val alu1 = Module(new AligendALU)
  val alu2 = Module(new AligendALU)
  val mdu  = Module(new AlignedMDU)
  val lsu  = Module(new AligendUnpipelinedLSU)
  val bru  = Module(new AligendALU) // TODO

  lsu.io.lsAXI <> io.lsAXI
  lsu.io.robCommit := io.robCommit

  alu1.io.in <> io.in(0)
  alu2.io.in <> io.in(1)
  mdu.io.in  <> io.in(2)
  lsu.io.in  <> io.in(3)
  bru.io.in  <> io.in(4)
  io.fire(0) := alu1.io.out.fire
  io.fire(1) := alu2.io.out.fire
  io.fire(2) := mdu.io.out.fire
  io.fire(3) := lsu.io.out.fire
  io.fire(4) := bru.io.out.fire
  io.out(0) <> alu1.io.out
  io.out(1) <> alu2.io.out
  io.out(2) <> mdu.io.out
  io.out(3) <> lsu.io.out
  io.out(4) <> bru.io.out
}