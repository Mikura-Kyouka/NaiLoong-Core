package core

import chisel3._
import chisel3.util._

class IFU2IDU extends Bundle {
  val pc = Output(UInt(32.W))
  val inst = Output(UInt(32.W))
  val Valid = Output(Bool())
  val brPredict = Output(new RedirectIO)
}

class IFU extends Module{
    val io = IO(new Bundle {
        val out   = Decoupled(Vec(4, new IFU2IDU))
        val axi   = new AXI
        
        val pcSel = Input(Bool())
        val flush = Input(Bool())
        val dnpc  = Input(UInt(32.W))
        val nextPC = Output(UInt(32.W))

        val BrPredictTaken = Vec(4, Flipped(new RedirectIO))

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
    val icache = Module(new PipelinedICache()(new ICacheConfig(totalSize = 1024 * 16, ways = 1))) // Pipelined
    io.out.valid := (pc.io.pc(1, 0) =/= 0.U || icache.io.out.valid) && !io.flush // TODO

    val predictValid = !RegNext(io.flush) && RegNext(icache.io.s1Fire)

    val predictTaken = io.BrPredictTaken.map(_.predictTaken).reduce(_ || _) && predictValid 
    val predictIndex = PriorityEncoder(io.BrPredictTaken.map(_.predictTaken))
    val predictTarget = PriorityMux(io.BrPredictTaken.map(_.predictTaken), io.BrPredictTaken.map(_.predictTarget))

    pc.io.PCSrc := io.pcSel
    pc.io.PCPredictTaken := predictTaken
    pc.io.dnpc := Mux(io.pcSel, io.dnpc, predictTarget)
    pc.io.stall := ~icache.io.s1Fire
    io.nextPC := pc.io.nextPC
    // io.out.bits.pc := icache.io.out.bits.addr

    icache.io.axi <> io.axi
    icache.io.out.ready := io.out.ready
    icache.io.in.addr := pc.io.pc

    val notValidPredict = Wire(new RedirectIO)
    notValidPredict.valid := false.B
    notValidPredict.predictTaken := false.B
    notValidPredict.predictTarget := 0.U
    notValidPredict.rtype := DontCare
    notValidPredict.actuallyTaken := false.B
    notValidPredict.actuallyTarget := 0.U

    icache.io.in.brPredictTaken := Mux(predictValid, io.BrPredictTaken, VecInit(Seq.fill(4)(notValidPredict)))

    icache.io.flush := io.flush
    icache.io.in.valid := io.out.ready && !io.flush // TODO
    // io.out.bits.inst := icache.io.out.bits.rdata
    val adef = Wire(new IFU2IDU)
    adef.Valid := true.B
    adef.pc := pc.io.pc
    adef.inst := 0x03400000.U
    adef.brPredict := DontCare
    val nop = Wire(new IFU2IDU)
    nop.Valid := false.B
    nop.pc := 0.U
    nop.inst := 0.U
    nop.brPredict := DontCare

    val hasBrPredictOut = icache.io.out.bits.map(x => x.brPredict.predictTaken && x.Valid).reduce(_ || _)
    val brPredictIdxOut = PriorityEncoder(icache.io.out.bits.map(x => x.brPredict.predictTaken && x.Valid))
    io.out.bits(0) := Mux(pc.io.pc(1, 0) =/= 0.U, adef, icache.io.out.bits(0))
    io.out.bits(1) := Mux(hasBrPredictOut && brPredictIdxOut < 1.U, nop, icache.io.out.bits(1))
    io.out.bits(2) := Mux(hasBrPredictOut && brPredictIdxOut < 2.U, nop, icache.io.out.bits(2))
    io.out.bits(3) := Mux(hasBrPredictOut && brPredictIdxOut < 3.U, nop, icache.io.out.bits(3))
}


