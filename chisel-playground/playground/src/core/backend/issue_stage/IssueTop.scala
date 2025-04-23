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
    // val cmtInstr = Flipped(Valid(new commit_inst_info))
    val cmtInstr = Input(Vec(5, Valid(UInt(PHYS_REG_BITS.W))))// FIXME: 5 -> ISSUE_WIDTH
    val rtrInstr = Flipped(Vec(4,Valid(new retire_inst_info)))
    val busy_info = Input(Vec(4, new busy_info))
  })

  val alu1rs = Module(new UnorderIssueQueue)
  val alu2rs = Module(new UnorderIssueQueue)
  val mdurs  = Module(new UnorderIssueQueue)
  val lsurs  = Module(new OrderIssueQueue)
  val brurs  = Module(new OrderIssueQueue)
  alu1rs.io.in <> io.in(0)
  alu1rs.io_raw := DontCare
  alu2rs.io.in <> io.in(1)
  alu2rs.io_raw.dest := io.out(0).bits.preg
  mdurs.io.in  <> io.in(2)
  mdurs.io_raw.dest := DontCare
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
  io.in(0).ready := alu1rs.io.in.ready
  io.in(1).ready := alu2rs.io.in.ready
  io.in(2).ready := mdurs.io.in.ready
  io.in(3).ready := lsurs.io.in.ready
  io.in(4).ready := brurs.io.in.ready

  // retire inst
  val busyreg = RegInit(VecInit(Seq.fill(PHYS_REG_NUM)(false.B)))
  for(i <- 0 until 4) {
    when(io.in(i).valid && io.in(i).ready && io.busy_info(i).valid && io.busy_info(i).preg =/= 0.U) {
      busyreg(io.busy_info(i).preg) := true.B
    }
  }
  for(i <- 0 until ISSUE_WIDTH) { // busyreg update
    when(io.out(i).valid && io.out(i).ready && io.out(i).bits.preg =/= 0.U) {
      busyreg(io.out(i).bits.preg) := true.B
    }
    // fu output update
    when(io.cmtInstr(i).valid) {
      busyreg(io.cmtInstr(i).bits) := false.B
    }
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

  for(i <- 0 until 4) {
    payloadram.io.write(i).dest := io.rtrInstr(i).bits.preg
    payloadram.io.write(i).pram_data := io.rtrInstr(i).bits.data
    payloadram.io.write(i).valid := io.rtrInstr(i).valid
  }


  if(GenCtrl.USE_DEBUG) {
    val alu1rs_debug = Module(new IssueDebug(1))
    val alu2rs_debug = Module(new IssueDebug(2))
    val mdurs_debug  = Module(new IssueDebug(3))
    val lsurs_debug  = Module(new IssueDebug(4))
    val brurs_debug  = Module(new IssueDebug(5))

    alu1rs_debug.io.valid := alu1rs.io.out.valid
    alu1rs_debug.io.inst_info := alu1rs.io.out.bits
    alu1rs_debug.io.ready := alu1rs.io.out.ready
    alu2rs_debug.io.valid := alu2rs.io.out.valid
    alu2rs_debug.io.inst_info := alu2rs.io.out.bits
    alu2rs_debug.io.ready := alu2rs.io.out.ready
    mdurs_debug.io.valid := mdurs.io.out.valid
    mdurs_debug.io.inst_info := mdurs.io.out.bits
    mdurs_debug.io.ready := mdurs.io.out.ready
    lsurs_debug.io.valid := lsurs.io.out.valid
    lsurs_debug.io.inst_info := lsurs.io.out.bits
    lsurs_debug.io.ready := lsurs.io.out.ready
    brurs_debug.io.valid := brurs.io.out.valid
    brurs_debug.io.inst_info := brurs.io.out.bits
    brurs_debug.io.ready := brurs.io.out.ready
  }
}