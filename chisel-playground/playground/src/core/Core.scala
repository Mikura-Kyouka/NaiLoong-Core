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

  // for (i <- 0 until 4) {
  //   Rn.io.rob.commit(i).valid := false.B
  //   Rn.io.rob.commit(i).bits := 0.U(RegConfig.PHYS_REG_BITS.W)
  // }

  PipelineConnect(If.io.to, Id.io.in, false.B, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Id.io.out, Rn.io.in, false.B, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Rn.io.out, Dispatch.io.in, false.B, rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(0), Issue.io.in(0), Issue.io.fire(0), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(1), Issue.io.in(1), Issue.io.fire(1), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(2), Issue.io.in(2), Issue.io.fire(2), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(3), Issue.io.in(3), Issue.io.fire(3), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Dispatch.io.out(4), Issue.io.in(4), Issue.io.fire(4), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(0), Ex.io.in(0), Ex.io.fire(0), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(1), Ex.io.in(1), Ex.io.fire(1), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(2), Ex.io.in(2), Ex.io.fire(2), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(3), Ex.io.in(3), Ex.io.fire(3), rob.io.brMisPredInfo.brMisPred.valid)
  PipelineConnect(Issue.io.out(4), Ex.io.in(4), Ex.io.fire(4), rob.io.brMisPredInfo.brMisPred.valid)

  If.io.intrpt := io.intrpt

  io.arid := If.io.arid
  io.araddr := If.io.araddr
  io.arlen := If.io.arlen
  io.arsize := If.io.arsize
  io.arburst := If.io.arburst
  io.arlock := If.io.arlock
  io.arcache := If.io.arcache
  io.arprot := If.io.arprot
  io.arvalid := If.io.arvalid
  If.io.arready := io.arready

  If.io.rid := io.rid
  If.io.rdata := io.rdata
  If.io.rresp := io.rresp
  If.io.rlast := io.rlast
  If.io.rvalid := io.rvalid
  io.rready := If.io.rready

  io.awid := If.io.awid
  io.awaddr := If.io.awaddr
  io.awlen := If.io.awlen
  io.awsize := If.io.awsize
  io.awburst := If.io.awburst
  io.awlock := If.io.awlock
  io.awcache := If.io.awcache
  io.awprot := If.io.awprot
  io.awvalid := If.io.awvalid
  If.io.awready := io.awready

  io.wid := If.io.wid
  io.wdata := If.io.wdata
  io.wstrb := If.io.wstrb
  io.wlast := If.io.wlast
  io.wvalid := If.io.wvalid
  If.io.wready := io.wready

  If.io.bid := io.bid
  If.io.bresp := io.bresp
  If.io.bvalid := io.bvalid
  io.bready := If.io.bready

  If.io.break_point := io.break_point
  If.io.infor_flag := io.infor_flag
  If.io.reg_num := io.reg_num
  io.ws_valid := If.io.ws_valid
  io.rf_rdata := If.io.rf_rdata

  io.debug0_wb_pc := If.io.debug0_wb_pc
  io.debug0_wb_rf_wen := If.io.debug0_wb_rf_wen
  io.debug0_wb_rf_wnum := If.io.debug0_wb_rf_wnum
  io.debug0_wb_rf_wdata := If.io.debug0_wb_rf_wdata

  io.debug1_wb_pc := If.io.debug1_wb_pc
  io.debug1_wb_rf_wen := If.io.debug1_wb_rf_wen
  io.debug1_wb_rf_wnum := If.io.debug1_wb_rf_wnum
  io.debug1_wb_rf_wdata := If.io.debug1_wb_rf_wdata

  If.io.flush := false.B
  If.io.new_pc := 0.U

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
  
  //dontTouch(Ex.io.out)

  Ex.io.out(0).ready := true.B
  Ex.io.out(1).ready := true.B
  Ex.io.out(2).ready := true.B
  Ex.io.out(3).ready := true.B
  Ex.io.out(4).ready := true.B
  
  // FIXME: ROB need 5 writeback channel
  for(i <- 0 until 5) {
    // rob entry <complete flag> update
    rob.io.writeback(i) := DontCare
    rob.io.writeback(i).valid := Ex.io.out(i).valid
    rob.io.writeback(i).bits.robIdx := Ex.io.out(i).bits.robIdx
    rob.io.writeback(i).bits.writeData := Ex.io.out(i).bits.data
    rob.io.writeback(i).bits.pc := Ex.io.out(i).bits.pc
    // <busy reg> update
    Issue.io.cmtInstr(i).valid := Ex.io.out(i).valid
    Issue.io.cmtInstr(i).bits := Ex.io.out(i).bits.robIdx
  }

  // allocate rob entries in rename stage
  Rn.io.robAllocate <> rob.io.allocate

  // arf and rat update
  Rn.io.rob <> rob.io.commit

  // branch handle logic
  Rn.io.brMispredict := rob.io.brMisPredInfo
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