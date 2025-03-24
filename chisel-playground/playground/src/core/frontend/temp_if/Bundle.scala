
import chisel3._
import chisel3.util._
class inst_info_ extends Bundle {
  val inst = UInt(32.W)
  val pc = UInt(32.W)
  val valid = Bool()
}

class if_to_id_info extends Bundle {
  val inst0 = new inst_info_
  val inst1 = new inst_info_
  val inst2 = new inst_info_
  val inst3 = new inst_info_
}

class axi4_info extends Bundle {
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
}