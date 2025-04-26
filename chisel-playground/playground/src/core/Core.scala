package core
import utils._
import chisel3._

class Core extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val intrpt   = Input(UInt(8.W))
    // AXI read address channel signals
    val arid     = Output(UInt(4.W)) 
    val araddr   = Output(UInt(32.W)) 
    val arlen    = Output(UInt(8.W)) 
    val arsize   = Output(UInt(3.W)) 
    val arburst  = Output(UInt(2.W)) 
    val arlock   = Output(UInt(2.W)) 
    val arcache  = Output(UInt(4.W)) 
    val arprot   = Output(UInt(3.W)) 
    val arvalid  = Output(Bool()) 
    val arready  = Input(Bool()) 

    // AXI read data channel signals
    val rid      = Input(UInt(4.W)) 
    val rdata    = Input(UInt(32.W)) 
    val rresp    = Input(UInt(2.W)) 
    val rlast    = Input(Bool()) 
    val rvalid   = Input(Bool()) 
    val rready   = Output(Bool()) 

    // AXI write address channel signals
    val awid     = Output(UInt(4.W)) 
    val awaddr   = Output(UInt(32.W)) 
    val awlen    = Output(UInt(8.W)) 
    val awsize   = Output(UInt(3.W)) 
    val awburst  = Output(UInt(2.W)) 
    val awlock   = Output(UInt(2.W)) 
    val awcache  = Output(UInt(4.W)) 
    val awprot   = Output(UInt(3.W)) 
    val awvalid  = Output(Bool()) 
    val awready  = Input(Bool()) 

    // AXI write data channel signals
    val wid      = Output(UInt(4.W)) 
    val wdata    = Output(UInt(32.W)) 
    val wstrb    = Output(UInt(4.W)) 
    val wlast    = Output(Bool()) 
    val wvalid   = Output(Bool()) 
    val wready   = Input(Bool()) 

    // AXI write response channel signals
    val bid      = Input(UInt(4.W)) 
    val bresp    = Input(UInt(2.W)) 
    val bvalid   = Input(Bool()) 
    val bready   = Output(Bool()) 

    val break_point=Input(Bool())
    val infor_flag =Input(Bool())
    val reg_num    =Input(UInt(5.W))
    val ws_valid   =Output(Bool())
    val rf_rdata   =Output(UInt(32.W))

    val debug0_wb_pc      =Output(UInt(32.W))
    val debug0_wb_rf_wen  =Output(UInt(4.W))
    val debug0_wb_rf_wnum =Output(UInt(5.W))
    val debug0_wb_rf_wdata=Output(UInt(32.W))
  
    val debug1_wb_pc      =Output(UInt(32.W))
    val debug1_wb_rf_wen  =Output(UInt(4.W))
    val debug1_wb_rf_wnum =Output(UInt(5.W))
    val debug1_wb_rf_wdata=Output(UInt(32.W))
  })

  val If = Module(new TempIf)
  val Id = Module(new IDU)
  val Rn = Module(new Rename)
  val Dispatch = Module(new Dispatch)
  val Issue = Module(new IssueTop)
  val Ex = Module(new Execute)

  val rob = Module(new Rob)

  val arb = Module(new Arb)

  // for (i <- 0 until 4) {
  //   Rn.io.rob.commit(i).valid := false.B
  //   Rn.io.rob.commit(i).bits := 0.U(RegConfig.PHYS_REG_BITS.W)
  // }

  PipelineConnect(If.io.to, Id.io.in, Id.io.in.fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Id.io.out, Rn.io.in, Rn.io.in.fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Rn.io.out, Dispatch.io.in, Dispatch.io.in.fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(0), Issue.io.in(0), Issue.io.in(0).fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(1), Issue.io.in(1), Issue.io.in(1).fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(2), Issue.io.in(2), Issue.io.in(2).fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(3), Issue.io.in(3), Issue.io.in(3).fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(4), Issue.io.in(4), Issue.io.in(4).fire, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(0), Ex.io.in(0), Ex.io.fire(0), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(1), Ex.io.in(1), Ex.io.fire(1), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(2), Ex.io.in(2), Ex.io.fire(2), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(3), Ex.io.in(3), Ex.io.fire(3), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(4), Ex.io.in(4), Ex.io.fire(4), rob.io.brMisPredInfo.brMisPred.valid)

  val ifAXI = Wire(new AXI)

  If.io.intrpt := io.intrpt

  ifAXI.arid := If.io.arid
  ifAXI.araddr := If.io.araddr
  ifAXI.arlen := If.io.arlen
  ifAXI.arsize := If.io.arsize
  ifAXI.arburst := If.io.arburst
  io.arlock := If.io.arlock
  io.arcache := If.io.arcache
  io.arprot := If.io.arprot
  ifAXI.arvalid := If.io.arvalid
  If.io.arready := ifAXI.arready

  If.io.rid := ifAXI.rid
  If.io.rdata := ifAXI.rdata
  If.io.rresp := ifAXI.rresp
  If.io.rlast := ifAXI.rlast
  If.io.rvalid := ifAXI.rvalid
  ifAXI.rready := If.io.rready

  ifAXI.awid := If.io.awid
  ifAXI.awaddr := If.io.awaddr
  ifAXI.awlen := If.io.awlen
  ifAXI.awsize := If.io.awsize
  ifAXI.awburst := If.io.awburst
  io.awlock := If.io.awlock
  io.awcache := If.io.awcache
  io.awprot := If.io.awprot
  ifAXI.awvalid := If.io.awvalid
  If.io.awready := ifAXI.awready

  io.wid := If.io.wid
  ifAXI.wdata := If.io.wdata
  ifAXI.wstrb := If.io.wstrb
  ifAXI.wlast := If.io.wlast
  ifAXI.wvalid := If.io.wvalid
  If.io.wready := ifAXI.wready

  If.io.bid := ifAXI.bid
  If.io.bresp := ifAXI.bresp
  If.io.bvalid := ifAXI.bvalid
  ifAXI.bready := If.io.bready

  If.io.break_point := io.break_point
  If.io.infor_flag := io.infor_flag
  If.io.reg_num := io.reg_num
  io.ws_valid := If.io.ws_valid
  io.rf_rdata := If.io.rf_rdata

  // If.io.intrpt := io.intrpt

  // io.arid := If.io.arid
  // io.araddr := If.io.araddr
  // io.arlen := If.io.arlen
  // io.arsize := If.io.arsize
  // io.arburst := If.io.arburst
  // io.arlock := If.io.arlock
  // io.arcache := If.io.arcache
  // io.arprot := If.io.arprot
  // io.arvalid := If.io.arvalid
  // If.io.arready := io.arready

  // If.io.rid := io.rid
  // If.io.rdata := io.rdata
  // If.io.rresp := io.rresp
  // If.io.rlast := io.rlast
  // If.io.rvalid := io.rvalid
  // io.rready := If.io.rready

  // io.awid := If.io.awid
  // io.awaddr := If.io.awaddr
  // io.awlen := If.io.awlen
  // io.awsize := If.io.awsize
  // io.awburst := If.io.awburst
  // io.awlock := If.io.awlock
  // io.awcache := If.io.awcache
  // io.awprot := If.io.awprot
  // io.awvalid := If.io.awvalid
  // If.io.awready := io.awready

  // io.wid := If.io.wid
  // io.wdata := If.io.wdata
  // io.wstrb := If.io.wstrb
  // io.wlast := If.io.wlast
  // io.wvalid := If.io.wvalid
  // If.io.wready := io.wready

  // If.io.bid := io.bid
  // If.io.bresp := io.bresp
  // If.io.bvalid := io.bvalid
  // io.bready := If.io.bready

  // If.io.break_point := io.break_point
  // If.io.infor_flag := io.infor_flag
  // If.io.reg_num := io.reg_num
  // io.ws_valid := If.io.ws_valid
  // io.rf_rdata := If.io.rf_rdata

  arb.io.ifu <> ifAXI
  arb.io.lsu <> Ex.io.lsAXI

  io.arid := arb.io.out.arid
  io.araddr := arb.io.out.araddr
  io.arlen := arb.io.out.arlen
  io.arsize := arb.io.out.arsize
  io.arburst := arb.io.out.arburst
  io.arvalid := arb.io.out.arvalid
  arb.io.out.arready := io.arready

  arb.io.out.rid := io.rid
  arb.io.out.rdata := io.rdata
  arb.io.out.rresp := io.rresp
  arb.io.out.rlast := io.rlast
  arb.io.out.rvalid := io.rvalid
  io.rready := arb.io.out.rready

  io.awid := arb.io.out.awid
  io.awaddr := arb.io.out.awaddr
  io.awlen := arb.io.out.awlen
  io.awsize := arb.io.out.awsize
  io.awburst := arb.io.out.awburst
  io.awvalid := arb.io.out.awvalid
  arb.io.out.awready := io.awready

  io.wdata := arb.io.out.wdata
  io.wstrb := arb.io.out.wstrb
  io.wlast := arb.io.out.wlast
  io.wvalid := arb.io.out.wvalid
  arb.io.out.wready := io.wready

  arb.io.out.bid := io.bid
  arb.io.out.bresp := io.bresp
  arb.io.out.bvalid := io.bvalid
  io.bready := arb.io.out.bready

  val traceBridge = Module(new TraceBridge)
  
  traceBridge.io.in_items := VecInit(rob.io.commit.commit.map { item =>
    WireInit({
    val trace = Wire(new TraceItem)
    trace.pc       := item.bits.pc
    trace.rf_we    := item.bits.dest =/= 0.U
    trace.rf_wnum  := item.bits.dest
    trace.rf_wdata := item.bits.data
    trace.valid    := item.valid
    trace
    })
  })

  traceBridge.io.in_valids := VecInit(rob.io.commitInstr.map { item =>
    WireInit({
    val trace = Wire(Bool())
    trace := item.valid
    trace
    })
  })

  traceBridge.io.out_ready := true.B 

  // 连接到 trace checker
  io.debug0_wb_pc       := traceBridge.io.out_item.pc
  io.debug0_wb_rf_wen    := traceBridge.io.out_item.rf_we
  io.debug0_wb_rf_wnum  := traceBridge.io.out_item.rf_wnum
  io.debug0_wb_rf_wdata := traceBridge.io.out_item.rf_wdata

  io.debug1_wb_pc := If.io.debug1_wb_pc
  io.debug1_wb_rf_wen := If.io.debug1_wb_rf_wen
  io.debug1_wb_rf_wnum := If.io.debug1_wb_rf_wnum
  io.debug1_wb_rf_wdata := If.io.debug1_wb_rf_wdata

  If.io.flush := rob.io.brMisPredInfo.brMisPred.valid
  If.io.new_pc := rob.io.brMisPredInfo.brMisPredTarget

  dontTouch(Rn.io.robAllocate)

  // Rn.io.out.ready := true.B
  // dontTouch(Rn.io.out)
  // dontTouch(Issue.io.out)
  // Issue.io.out(0).ready := true.B
  // Issue.io.out(1).ready := true.B
  // Issue.io.out(2).ready := true.B
  // Issue.io.out(3).ready := true.B
  // Issue.io.out(4).ready := true.B

  Issue.io.cmtInstr := DontCare
  Issue.io.rtrInstr := DontCare
  Issue.io.busy_info := Dispatch.io.busy_info
  for(i <- 0 until 5) {
    Issue.io.ex_bypass(i).valid := Ex.io.out(i).valid
    Issue.io.ex_bypass(i).dest := Ex.io.in(i).bits.preg
    Issue.io.ex_bypass(i).data := Ex.io.out(i).bits.data
  }

  for(i <- 0 until 4) {
    Issue.io.rtrInstr(i).bits.data := rob.io.commit.commit(i).bits.data
    Issue.io.rtrInstr(i).valid := rob.io.commitInstr(i).valid
    Issue.io.rtrInstr(i).bits.preg := rob.io.commit.commit(i).bits.preg
  }
  
  Ex.io.out(0).ready := true.B
  Ex.io.out(1).ready := true.B
  Ex.io.out(2).ready := true.B
  Ex.io.out(3).ready := true.B
  Ex.io.out(4).ready := true.B
  
  // FIXME: ROB need 5 writeback channel
  dontTouch(Ex.io.out)
  for(i <- 0 until 5) {
    // rob entry <complete flag> update
    rob.io.writeback(i) := DontCare
    rob.io.writeback(i).valid := Ex.io.out(i).valid
    rob.io.writeback(i).bits.robIdx := Ex.io.out(i).bits.robIdx
    rob.io.writeback(i).bits.writeData := Ex.io.out(i).bits.data
    rob.io.writeback(i).bits.pc := Ex.io.out(i).bits.pc
    rob.io.writeback(i).bits.brMispredict := Ex.io.out(i).bits.redirect.valid
    rob.io.writeback(i).bits.brTarget := Ex.io.out(i).bits.redirect.target

    // <busy reg> update
    Issue.io.cmtInstr(i).valid := Ex.io.out(i).valid
    Issue.io.cmtInstr(i).bits := Ex.io.out(i).bits.robIdx
  }

  // allocate rob entries in rename stage
  Rn.io.robAllocate <> rob.io.allocate

  // lsu <=> rob
  Ex.io.robCommit := rob.io.commit
  // arf and rat update
  Rn.io.rob <> rob.io.commit

  // branch handle logic
  Rn.io.brMispredict := rob.io.brMisPredInfo
  
  // difftest
  if(GenCtrl.USE_DIFF) {
    val DiffCommit = Module(new DiffCommit)

    DiffCommit.io.instr(0).valid := rob.io.commitInstr(0).valid
    DiffCommit.io.instr(0).pc := rob.io.commitPC(0).bits
    DiffCommit.io.instr(0).instr := rob.io.commitInstr(0).bits
    DiffCommit.io.instr(0).skip := DontCare
    DiffCommit.io.instr(0).is_TLBFILL := DontCare
    DiffCommit.io.instr(0).TLBFILL_index := DontCare
    DiffCommit.io.instr(0).is_CNTinst := DontCare
    DiffCommit.io.instr(0).timer_64_value := DontCare
    DiffCommit.io.instr(0).wen := rob.io.commit.commit(0).bits.dest =/= 0.U
    DiffCommit.io.instr(0).wdest := rob.io.commit.commit(0).bits.dest
    DiffCommit.io.instr(0).wdata := rob.io.commit.commit(0).bits.data
    DiffCommit.io.instr(0).csr_rstat := DontCare
    DiffCommit.io.instr(0).csr_data := DontCare

    DiffCommit.io.instr(1).valid := rob.io.commitInstr(1).valid
    DiffCommit.io.instr(1).pc := rob.io.commitPC(1).bits
    DiffCommit.io.instr(1).instr := rob.io.commitInstr(1).bits
    DiffCommit.io.instr(1).skip := DontCare
    DiffCommit.io.instr(1).is_TLBFILL := DontCare
    DiffCommit.io.instr(1).TLBFILL_index := DontCare
    DiffCommit.io.instr(1).is_CNTinst := DontCare
    DiffCommit.io.instr(1).timer_64_value := DontCare
    DiffCommit.io.instr(1).wen := rob.io.commit.commit(1).bits.dest =/= 0.U
    DiffCommit.io.instr(1).wdest := rob.io.commit.commit(1).bits.dest
    DiffCommit.io.instr(1).wdata := rob.io.commit.commit(1).bits.data
    DiffCommit.io.instr(1).csr_rstat := DontCare
    DiffCommit.io.instr(1).csr_data := DontCare

    DiffCommit.io.instr(2).valid := rob.io.commitInstr(2).valid
    DiffCommit.io.instr(2).pc := rob.io.commitPC(2).bits
    DiffCommit.io.instr(2).instr := rob.io.commitInstr(2).bits
    DiffCommit.io.instr(2).skip := DontCare
    DiffCommit.io.instr(2).is_TLBFILL := DontCare
    DiffCommit.io.instr(2).TLBFILL_index := DontCare
    DiffCommit.io.instr(2).is_CNTinst := DontCare
    DiffCommit.io.instr(2).timer_64_value := DontCare
    DiffCommit.io.instr(2).wen := rob.io.commit.commit(2).bits.dest =/= 0.U
    DiffCommit.io.instr(2).wdest := rob.io.commit.commit(2).bits.dest
    DiffCommit.io.instr(2).wdata := rob.io.commit.commit(2).bits.data
    DiffCommit.io.instr(2).csr_rstat := DontCare
    DiffCommit.io.instr(2).csr_data := DontCare

    DiffCommit.io.instr(3).valid := rob.io.commitInstr(3).valid
    DiffCommit.io.instr(3).pc := rob.io.commitPC(3).bits
    DiffCommit.io.instr(3).instr := rob.io.commitInstr(3).bits
    DiffCommit.io.instr(3).skip := DontCare
    DiffCommit.io.instr(3).is_TLBFILL := DontCare
    DiffCommit.io.instr(3).TLBFILL_index := DontCare
    DiffCommit.io.instr(3).is_CNTinst := DontCare
    DiffCommit.io.instr(3).timer_64_value := DontCare
    DiffCommit.io.instr(3).wen := rob.io.commit.commit(3).bits.dest =/= 0.U
    DiffCommit.io.instr(3).wdest := rob.io.commit.commit(3).bits.dest
    DiffCommit.io.instr(3).wdata := rob.io.commit.commit(3).bits.data
    DiffCommit.io.instr(3).csr_rstat := DontCare
    DiffCommit.io.instr(3).csr_data := DontCare

    DiffCommit.io.reg := Rn.io.arf
  }
}

object GenFr extends App {
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
    circt.stage.ChiselStage.emitSystemVerilogFile(new Core(), args, firtoolOptions)
}