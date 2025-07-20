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
    val rtrInstr = Flipped(Vec(RobConfig.ROB_CMT_NUM, Valid(new retire_inst_info)))
    val ex_bypass = Input(Vec(5, new bypass_info))
    val flush = Input(Bool())
  })

  val alu1rs = Module(new UnorderIssueQueue(check_dest = false, SIZE = UNORDER_QUEUE_SIZE, MAX_CNT = 2))
  val alu2rs = Module(new UnorderIssueQueue(check_dest = false, SIZE = UNORDER_QUEUE_SIZE, MAX_CNT = 2))
  val mdurs  = Module(new UnorderIssueQueue(check_dest = false, SIZE = UNORDER_QUEUE_SIZE, MAX_CNT = 2))
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
  val all_ready = alu1rs.io.in.ready && alu2rs.io.in.ready && 
                  mdurs.io.in.ready && lsurs.io.in.ready && brurs.io.in.ready
  io.in(0).ready := all_ready
  io.in(1).ready := all_ready
  io.in(2).ready := all_ready
  io.in(3).ready := all_ready
  io.in(4).ready := all_ready
  alu1rs.io.flush := io.flush
  alu2rs.io.flush := io.flush
  mdurs.io.flush := io.flush
  lsurs.io.flush := io.flush
  brurs.io.flush := io.flush

  val busyreg = RegInit(VecInit(Seq.fill(PHYS_REG_NUM)(false.B)))
  for(i <- 0 until ISSUE_WIDTH) {
    when(io.in(i).valid) {
      for(j <- 0 until 4) {
        when(io.in(i).bits.inst_vec(j).preg =/= 0.U && j.U < io.in(i).bits.inst_cnt) {
          busyreg(io.in(i).bits.inst_vec(j).preg) := true.B
        }
      }
    }
  }
  
  when(io.flush) {
    for(i <- 0 until PHYS_REG_NUM) {
      busyreg(i) := false.B
    }
  }

  // retire inst
  for(i <- 0 until RobConfig.ROB_CMT_NUM) { // busyreg update
    // fu output update
    when(io.rtrInstr(i).valid) {
      busyreg(io.rtrInstr(i).bits.preg) := false.B
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

  for(i <- 0 until 5) {
    // val prev_valid = RegNext(io.ex_bypass(i).valid)
    // val bypass_fire = io.ex_bypass(i).valid && !prev_valid
    payloadram.io.write(i).dest := io.ex_bypass(i).dest
    payloadram.io.write(i).pram_data := io.ex_bypass(i).data
    payloadram.io.write(i).valid := io.ex_bypass(i).valid
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