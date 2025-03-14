import chisel3._

class TempIf extends Module {
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

  val pc = RegInit("h1c000000".U(32.W))

  val idle :: read :: nop :: Nil = Enum(3)
  val state = RegInit(idle)

  // AXI signals
  val arvalid = RegInit(true.B)
  io.arvalid := arvalid
  val rready = RegInit(false.B)
  io.rready := rready

  // temp inst
  val inst = RegInit(0.U(32.W))
  dontTouch(inst)

  // connect interface signals
  io.arid := 0.U
  io.araddr := pc
  io.arlen := 0.U
  io.arsize := 0.U
  io.arburst := 0.U
  io.arlock := 0.U
  io.arcache := 0.U
  io.arprot := 0.U

  io.awid := 0.U
  io.awaddr := pc
  io.awlen := 0.U
  io.awsize := 0.U
  io.awburst := 0.U
  io.awlock := 0.U
  io.awcache := 0.U
  io.awprot := 0.U
  io.awvalid := false.B

  io.wid := 0.U
  io.wdata := 0.U
  io.wstrb := 0.U
  io.wlast := false.B
  io.wvalid := false.B
  
  io.bready := false.B

  io.ws_valid := false.B
  io.rf_rdata := 0.U
  io.debug0_wb_pc := 0.U
  io.debug0_wb_rf_wen := 0.U
  io.debug0_wb_rf_wnum := 0.U
  io.debug0_wb_rf_wdata := 0.U
  io.debug1_wb_pc := 0.U
  io.debug1_wb_rf_wen := 0.U
  io.debug1_wb_rf_wnum := 0.U
  io.debug1_wb_rf_wdata := 0.U

  val cache_wen = RegInit(false.B)
  val cache_waddr = RegInit(0.U(32.W))
  val cache_wdata = RegInit(0.U(32.W))

  val temp_cache = Module(new TempIcache)
  temp_cache.io.waddr := cache_waddr
  temp_cache.io.wdata := cache_wdata
  temp_cache.io.wen := cache_wen

  switch(state) {
    is(idle) {
      when(io.arvalid && io.arready) {
        state := nop
        arvalid := false.B
        rready := true.B
      }.otherwise {
        arvalid := true.B
      }
      cache_wen := false.B
    }
    is(nop) {
      state := read
    }
    is(read) {
      when(io.rvalid && io.rready) {
        state := idle
        rready := false.B
        arvalid := true.B
        inst := io.rdata
        pc := pc + 4.U

        cache_wen := true.B
        cache_waddr := pc
        cache_wdata := io.rdata
      }.otherwise {
        rready := true.B
      }
    }
  }
}

object GenIF extends App {
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
    circt.stage.ChiselStage.emitSystemVerilogFile(new TempIf(), args, firtoolOptions)
}