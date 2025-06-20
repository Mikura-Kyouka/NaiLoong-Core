package core

import chisel3._
import chisel3.util._

class IFU2IDU extends Bundle {
  val pc = Output(UInt(32.W))
  val inst = Output(UInt(32.W))
  val Valid = Output(Bool())
  val brPredictTaken = Output(Bool())
}

class IFU extends Module{
    val io = IO(new Bundle {
        val out   = Decoupled(Vec(4, new IFU2IDU))
        val axi   = new AXI
        
        val pcSel = Input(Bool())
        val flush = Input(Bool())
        val dnpc  = Input(UInt(32.W))
        val nextPC = Output(UInt(32.W))

        val BrPredictTaken = Input(Vec(4, Bool()))

        val intrpt = Input(Bool())

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

    io.axi := DontCare
    /* Full AXI4 initialization */
    io.axi.arid := 0.U(4.W)
    io.axi.awid := 0.U(4.W)
    io.axi.awlen := 0.U(8.W)
    io.axi.awsize := 0.U(3.W)
    io.axi.awburst := 0.U(2.W)
    io.axi.wlast := true.B
    io.axi.awvalid := false.B
    io.axi.bready := false.B
    io.axi.wvalid := false.B
    io.axi.wid := 0.U(4.W)
    io.axi.wdata := 0.U
    io.axi.wstrb := 0.U
    io.axi.awaddr := 0.U

    val pc = Module(new PC())
    val icache = Module(new PipelinedICache()(new ICacheConfig(totalSize = 32 * 16, ways = 1))) // Pipelined
    io.out.valid := (pc.io.pc(1, 0) =/= 0.U || icache.io.out.valid) && !io.flush // TODO
    pc.io.PCSrc := io.pcSel
    pc.io.dnpc := io.dnpc
    pc.io.stall :=  ~icache.io.s1Fire
    io.nextPC := pc.io.pc
    // io.out.bits.pc := icache.io.out.bits.addr

    icache.io.axi <> io.axi
    icache.io.out.ready := io.out.ready
    icache.io.in.addr := pc.io.pc
    icache.io.in.brPredictTaken := io.BrPredictTaken

    icache.io.flush := io.flush 
    icache.io.in.valid := io.out.ready && !io.flush // TODO
    // io.out.bits.inst := icache.io.out.bits.rdata
    val adef = Wire(new IFU2IDU)
    adef.Valid := true.B
    adef.pc := pc.io.pc
    adef.inst := 0x03400000.U
    adef.brPredictTaken := false.B
    io.out.bits(0) := Mux(pc.io.pc(1, 0) =/= 0.U, adef, icache.io.out.bits(0))
    io.out.bits(1) := icache.io.out.bits(1)
    io.out.bits(2) := icache.io.out.bits(2)
    io.out.bits(3) := icache.io.out.bits(3)
}


