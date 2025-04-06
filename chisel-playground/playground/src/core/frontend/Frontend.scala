package core
import chisel3._
class Frontend extends Module {
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

  // class CtrlFlowIO extends Bundle {
  //   val instr = Output(UInt(32.W))
  //   val pc = Output(UInt(32.W)) // TODO:VAddrBits
  //   val pnpc = Output(UInt(32.W)) // TODO:VAddrBits
  //   val redirect = new RedirectIO
  //   val exceptionVec = Output(Vec(16, Bool()))
  //   val intrVec = Output(Vec(12, Bool()))
  //   val brIdx = Output(UInt(4.W))
  //   val crossPageIPFFix = Output(Bool())
  //   val runahead_checkpoint_id = Output(UInt(64.W))
  //   val isBranch = Output(Bool())
  // }

  val If = Module(new TempIf)
  val Id0 = Module(new Decoder)
  Id0.io.in.bits.pc := If.io.to.bits.inst0.pc
  Id0.io.in.bits.instr := If.io.to.bits.inst0.inst
  Id0.io.in.valid := If.io.to.bits.inst0.valid
  Id0.io.in.bits.pnpc := DontCare
  Id0.io.in.bits.redirect := DontCare
  Id0.io.in.bits.exceptionVec := DontCare
  Id0.io.in.bits.intrVec := DontCare
  Id0.io.in.bits.brIdx := DontCare
  Id0.io.in.bits.crossPageIPFFix := DontCare
  Id0.io.in.bits.runahead_checkpoint_id := DontCare
  Id0.io.in.bits.isBranch := DontCare
  Id0.io.out.ready := true.B
  dontTouch(Id0.io.out.bits)
  val Id1 = Module(new Decoder)
  Id1.io.in.bits.pc := If.io.to.bits.inst1.pc
  Id1.io.in.bits.instr := If.io.to.bits.inst1.inst
  Id1.io.in.valid := If.io.to.bits.inst1.valid
  Id1.io.in.bits.pnpc := DontCare
  Id1.io.in.bits.redirect := DontCare
  Id1.io.in.bits.exceptionVec := DontCare
  Id1.io.in.bits.intrVec := DontCare
  Id1.io.in.bits.brIdx := DontCare
  Id1.io.in.bits.crossPageIPFFix := DontCare
  Id1.io.in.bits.runahead_checkpoint_id := DontCare
  Id1.io.in.bits.isBranch := DontCare
  Id1.io.out.ready := true.B
  dontTouch(Id1.io.out.bits)
  val Id2 = Module(new Decoder)
  Id2.io.in.bits.pc := If.io.to.bits.inst2.pc
  Id2.io.in.bits.instr := If.io.to.bits.inst2.inst
  Id2.io.in.valid := If.io.to.bits.inst2.valid
  Id2.io.in.bits.pnpc := DontCare
  Id2.io.in.bits.redirect := DontCare
  Id2.io.in.bits.exceptionVec := DontCare
  Id2.io.in.bits.intrVec := DontCare
  Id2.io.in.bits.brIdx := DontCare
  Id2.io.in.bits.crossPageIPFFix := DontCare
  Id2.io.in.bits.runahead_checkpoint_id := DontCare
  Id2.io.in.bits.isBranch := DontCare
  Id2.io.out.ready := true.B
  dontTouch(Id2.io.out.bits)
  val Id3 = Module(new Decoder)
  Id3.io.in.bits.pc := If.io.to.bits.inst3.pc
  Id3.io.in.bits.instr := If.io.to.bits.inst3.inst
  Id3.io.in.valid := If.io.to.bits.inst3.valid
  Id3.io.in.bits.pnpc := DontCare
  Id3.io.in.bits.redirect := DontCare
  Id3.io.in.bits.exceptionVec := DontCare
  Id3.io.in.bits.intrVec := DontCare
  Id3.io.in.bits.brIdx := DontCare
  Id3.io.in.bits.crossPageIPFFix := DontCare
  Id3.io.in.bits.runahead_checkpoint_id := DontCare
  Id3.io.in.bits.isBranch := DontCare
  Id3.io.out.ready := true.B
  dontTouch(Id3.io.out.bits)

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
  If.io.to.ready := true.B
}