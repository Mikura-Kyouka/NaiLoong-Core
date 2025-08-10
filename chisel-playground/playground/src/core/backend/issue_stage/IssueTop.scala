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
    val in_allReady = Output(Bool())

    val inst_cnt = Vec(ISSUE_WIDTH, Input(UInt(3.W)))

    val fire = Vec(ISSUE_WIDTH, Output(Bool()))
    val rtrInstr = Flipped(Vec(RobConfig.ROB_CMT_NUM, Valid(new retire_inst_info)))
    val ex_bypass = Input(Vec(5, new bypass_info))
    val flush = Input(Bool())
  })

  val alu1rs = Module(new UnorderIssueQueue(wakeup = true, SIZE = UNORDER_QUEUE_SIZE1, MAX_CNT = 2))
  val alu2rs = Module(new UnorderIssueQueue(wakeup = true, SIZE = UNORDER_QUEUE_SIZE2, MAX_CNT = 2))
  val mdurs  = Module(new UnorderIssueQueue(wakeup = false, SIZE = MDU_QUEUE_SIZE, MAX_CNT = 4))
  val lsurs  = Module(new OrderIssueQueue(SIZE = 8, MAX_CNT = 4))
  val brurs  = Module(new OrderIssueQueue(SIZE = 6, MAX_CNT = 4))
  alu1rs.io.in <> io.in(0)
  alu1rs.io_raw := DontCare
  alu1rs.io.inst_cnt := io.inst_cnt(0)
  alu2rs.io.in <> io.in(1)
  alu2rs.io_raw.dest := io.out(0).bits.preg
  alu2rs.io.inst_cnt := io.inst_cnt(1)
  mdurs.io.in  <> io.in(2)
  mdurs.io_raw.dest := DontCare
  mdurs.io.inst_cnt := io.inst_cnt(2)
  lsurs.io.in  <> io.in(3)
  lsurs.io.inst_cnt := io.inst_cnt(3)
  brurs.io.in  <> io.in(4)
  val all_ready = alu1rs.io.in.ready && alu2rs.io.in.ready && 
                  mdurs.io.in.ready && lsurs.io.in.ready && brurs.io.in.ready
  alu1rs.io.allReady := all_ready
  alu2rs.io.allReady := all_ready
  mdurs.io.allReady := all_ready
  lsurs.io.allReady := all_ready
  brurs.io.allReady := all_ready
  brurs.io.inst_cnt := io.inst_cnt(4)
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
  io.in_allReady := all_ready

  val busyreg = RegInit(VecInit(Seq.fill(PHYS_REG_NUM + 1)(false.B)))
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
    for(i <- 0 until PHYS_REG_NUM + 1) {
      busyreg(i) := false.B
    }
  }

  // // retire inst
  // for(i <- 0 until RobConfig.ROB_CMT_NUM) { // busyreg update
  //   // fu output update
  //   when(io.rtrInstr(i).valid) {
  //     busyreg(io.rtrInstr(i).bits.preg) := false.B
  //   }
  // }

  for(i <- 0 until 5) {
    when(io.ex_bypass(i).valid) {
      busyreg(io.ex_bypass(i).dest) := false.B
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

  if(GenCtrl.USE_COUNT) {
    // 1. 周期计数器
    val cycle_counter = RegInit(0.U(32.W))
    cycle_counter := cycle_counter + 1.U

    // 2. 为每个RS添加低电平计数器
    val alu1rs_not_ready_counter = RegInit(0.U(32.W))
    val alu2rs_not_ready_counter = RegInit(0.U(32.W))
    val mdurs_not_ready_counter = RegInit(0.U(32.W))
    val lsurs_not_ready_counter = RegInit(0.U(32.W))
    val brurs_not_ready_counter = RegInit(0.U(32.W))

    // 3. 计数器更新逻辑
    when(!alu1rs.io.in.ready) {
      alu1rs_not_ready_counter := alu1rs_not_ready_counter + 1.U
    }
    when(!alu2rs.io.in.ready) {
      alu2rs_not_ready_counter := alu2rs_not_ready_counter + 1.U
    }
    when(!mdurs.io.in.ready) {
      mdurs_not_ready_counter := mdurs_not_ready_counter + 1.U
    }
    when(!lsurs.io.in.ready) {
      lsurs_not_ready_counter := lsurs_not_ready_counter + 1.U
    }
    when(!brurs.io.in.ready) {
      brurs_not_ready_counter := brurs_not_ready_counter + 1.U
    }

    // 4. 每隔500周期打印统计信息
    when(cycle_counter % 500.U === 0.U && cycle_counter =/= 0.U) {
      printf("----------------------------------------\n") 
      printf(p"Cycle ${cycle_counter}: RS Stall Statistics\n")
      printf(p"  ALU1RS: ${alu1rs_not_ready_counter} cycles\n")
      printf(p"  ALU2RS: ${alu2rs_not_ready_counter} cycles\n")
      printf(p"  MDURS:  ${mdurs_not_ready_counter} cycles\n")
      printf(p"  LSURS:  ${lsurs_not_ready_counter} cycles\n")
      printf(p"  BRURS:  ${brurs_not_ready_counter} cycles\n")
      printf("----------------------------------------\n")
    }
  }
}