package core
import chisel3._
import IssueConfig._
import chisel3.util._
import chisel3.stage._
import utils.PipelineConnect

class IssueTop extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(ISSUE_WIDTH, Decoupled(new dispatch_out_info)))
    val out = Vec(ISSUE_WIDTH, Decoupled(new PipelineConnectIO))

    val fire = Vec(ISSUE_WIDTH, Output(Bool()))
    val cmtInstr = Flipped(Valid(new commit_inst_info))
    val rtrInstr = Flipped(Valid(new retire_inst_info))
  })

  val alu1rs = Module(new UnorderIssueQueue)
  val alu2rs = Module(new UnorderIssueQueue)
  val mdurs  = Module(new UnorderIssueQueue)
  val lsurs  = Module(new OrderIssueQueue)
  val brurs  = Module(new OrderIssueQueue)
  alu1rs.io.in <> io.in(0)
  alu2rs.io.in <> io.in(1)
  mdurs.io.in  <> io.in(2)
  lsurs.io.in  <> io.in(3)
  brurs.io.in  <> io.in(4)
  io.fire(0) := alu1rs.io.out.fire
  io.fire(1) := alu2rs.io.out.fire
  io.fire(2) := mdurs.io.out.fire
  io.fire(3) := lsurs.io.out.fire
  io.fire(4) := brurs.io.out.fire
  io.out(0) <> alu1rs.io.out
  io.out(1) <> alu2rs.io.out
  io.out(2) <> mdurs.io.out
  io.out(3) <> lsurs.io.out
  io.out(4) <> brurs.io.out

  // retire inst
  val busyreg = RegInit(VecInit(Seq.fill(PHYS_REG_NUM)(false.B)))
  for(i <- 0 until ISSUE_WIDTH) {
    when(io.out(i).valid && io.out(i).ready) {
      busyreg(io.out(i).bits.preg) := true.B
    }
  }
  when(io.cmtInstr.valid) {
    busyreg(io.cmtInstr.bits.inst.dest) := false.B
  }

  // Connect busy signal
  alu1rs.io.busyreg := busyreg
  alu2rs.io.busyreg := busyreg
  mdurs.io.busyreg := busyreg
  lsurs.io.busyreg := busyreg
  brurs.io.busyreg := busyreg

  // Connect payloadram
  val payloadram = Module(new PayloadRAM)
  alu1rs.io.pram_read <> payloadram.io.read(0)
  alu2rs.io.pram_read <> payloadram.io.read(1)
  mdurs.io.pram_read <> payloadram.io.read(2)
  lsurs.io.pram_read <> payloadram.io.read(3)
  brurs.io.pram_read <> payloadram.io.read(4)
  payloadram.io.write.dest := io.rtrInstr.bits.preg
  payloadram.io.write.pram_data := io.rtrInstr.bits.data
  payloadram.io.write.valid := io.rtrInstr.valid
}