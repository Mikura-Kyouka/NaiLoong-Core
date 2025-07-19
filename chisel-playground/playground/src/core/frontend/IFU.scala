package core

import chisel3._
import chisel3.util._

class fetchExcp extends Bundle {
  val en = Bool() // 是否发生异常
  val ecode = UInt(4.W) // 异常码
}

class IFU2IDU extends Bundle {
  val pc = Output(UInt(32.W))
  val inst = Output(UInt(32.W))
  val Valid = Output(Bool())
  val brPredict = Output(new RedirectIO)
  val excp = Output(new fetchExcp) // TLB reload excp
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

        val addr_trans_out = Output(new AddrTrans)
        val addr_trans_in = Flipped(Decoupled(new AddrTrans))

        // cacop signal
        val cacop = Input(new CACOPIO)
        val excp_en = Output(Bool())
        val ecode = Output(Ecode())

        val idle = Input(Bool())

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
    val icache = Module(new PipelinedICache()(new ICacheConfig(totalSize = 512 * 16, ways = 1))) // Pipelined
    io.out.valid := (pc.io.pc(1, 0) =/= 0.U || icache.io.out.valid || io.addr_trans_in.valid && io.addr_trans_in.bits.excp.en) && 
                    !io.flush && !icache.io.s1Cacop && !io.idle

    val predictTaken = io.BrPredictTaken.map(_.predictTaken).reduce(_ || _)
    val predictIndex = PriorityEncoder(io.BrPredictTaken.map(_.predictTaken))
    val predictTarget = PriorityMux(io.BrPredictTaken.map(_.predictTaken), io.BrPredictTaken.map(_.predictTarget))
    val predictInfo = io.BrPredictTaken
    
    val predictTakenReg = RegInit(false.B)
    val predictIndexReg = RegInit(0.U(2.W))
    val predictTargetReg = RegInit("h00000000".U(32.W))
    val predictInfoReg = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(new RedirectIO))))

    when(icache.io.s1Fire || io.flush) {
        predictTakenReg := false.B
    }
    when(predictTaken && !icache.io.s1Fire && !io.flush) {
        predictTakenReg := true.B
        predictIndexReg := predictIndex
        predictTargetReg := predictTarget
        predictInfoReg := predictInfo
    }

    pc.io.PCSrc := io.pcSel
    pc.io.PCPredictTaken := predictTaken || predictTakenReg && !icache.io.s1Fire
    pc.io.dnpc := Mux(io.pcSel, io.dnpc, Mux(predictTaken, predictTarget, predictTargetReg))
    pc.io.stall := ~icache.io.s1Fire // s1Fire，注意：如果是cacop指令，s1的valid是不会拉高的
    io.nextPC := pc.io.nextPC
    // io.out.bits.pc := icache.io.out.bits.addr

    io.addr_trans_out := DontCare
    io.addr_trans_out.vaddr := Mux(io.cacop.en && io.cacop.op === CACOPOp.op2, io.cacop.VA, pc.io.nextPC)
    io.addr_trans_out.mem_type := Mux(io.cacop.en && io.cacop.op === CACOPOp.op2, MemType.load, MemType.fetch)
    io.addr_trans_out.trans_en := true.B
    io.addr_trans_in.ready := true.B

    icache.io.axi <> io.axi
    icache.io.out.ready := io.out.ready
    icache.io.in.addr := io.addr_trans_in.bits.paddr
    icache.io.in.pc := RegNext(io.addr_trans_out.vaddr)
    icache.io.in.mat := io.addr_trans_in.bits.mat
    icache.io.in.cacop := io.cacop

    io.excp_en := io.addr_trans_in.bits.excp.en
    io.ecode := io.addr_trans_in.bits.excp.ecode

    val notValidPredict = Wire(new RedirectIO)
    notValidPredict.valid := false.B
    notValidPredict.predictTaken := false.B
    notValidPredict.predictTarget := 0.U
    notValidPredict.rtype := DontCare
    notValidPredict.actuallyTaken := false.B
    notValidPredict.actuallyTarget := 0.U

    icache.io.in.brPredictTaken := Mux(predictTaken, io.BrPredictTaken, 
                                       Mux(predictTakenReg, predictInfoReg, 
                                           VecInit(Seq.fill(4)(notValidPredict))))

    icache.io.flush := io.flush
    icache.io.in.valid := io.out.ready && !io.flush && !io.addr_trans_in.bits.excp.en && io.addr_trans_in.valid && !io.idle // TODO
    // io.out.bits.inst := icache.io.out.bits.rdata
    val adef = Wire(new IFU2IDU)
    adef.Valid := true.B
    adef.pc := pc.io.pc
    adef.inst := 0x03400000.U
    adef.brPredict := DontCare
    adef.excp := DontCare
    val nop = Wire(new IFU2IDU)
    nop.Valid := false.B
    nop.pc := 0.U
    nop.inst := 0.U
    nop.brPredict := DontCare
    nop.excp := DontCare
    val nopWithExcp = Wire(new IFU2IDU)
    nopWithExcp.Valid := true.B
    nopWithExcp.pc := RegNext(io.addr_trans_out.vaddr)
    nopWithExcp.inst := 0x03400000.U
    nopWithExcp.brPredict := DontCare
    nopWithExcp.excp.en := true.B
    nopWithExcp.excp.ecode := io.addr_trans_in.bits.excp.ecode

    val hasBrPredictOut = icache.io.out.bits.map(x => x.brPredict.predictTaken && x.Valid).reduce(_ || _)
    val brPredictIdxOut = PriorityEncoder(icache.io.out.bits.map(x => x.brPredict.predictTaken && x.Valid))

    val out_temp = WireInit(icache.io.out.bits)

    out_temp(0) := Mux(io.addr_trans_in.bits.excp.en, nopWithExcp, icache.io.out.bits(0))
    out_temp(1) := Mux(io.addr_trans_in.bits.excp.en, nopWithExcp, icache.io.out.bits(1))
    out_temp(2) := Mux(io.addr_trans_in.bits.excp.en, nopWithExcp, icache.io.out.bits(2))
    out_temp(3) := Mux(io.addr_trans_in.bits.excp.en, nopWithExcp, icache.io.out.bits(3))

    io.out.bits(0) := Mux(pc.io.pc(1, 0) =/= 0.U, adef, out_temp(0))
    io.out.bits(1) := Mux(hasBrPredictOut && brPredictIdxOut < 1.U, nop, out_temp(1))
    io.out.bits(2) := Mux(hasBrPredictOut && brPredictIdxOut < 2.U, nop, out_temp(2))
    io.out.bits(3) := Mux(hasBrPredictOut && brPredictIdxOut < 3.U, nop, out_temp(3))

    if(GenCtrl.USE_COUNT) {
      val counting = RegInit(false.B)
      when(icache.io.in.valid) {
        counting := true.B
      }.elsewhen(io.out.fire || io.flush) {
        counting := false.B
      }

      val delayCounter = RegInit(0.U(64.W))
      val ifCounter = RegInit(1.U(32.W))

      when(io.out.fire) {
        ifCounter := ifCounter + 1.U
        printf("[IFU] Average IFU delay: %d cycles\n", delayCounter / ifCounter)
      }.elsewhen(counting) {
        delayCounter := delayCounter + 1.U
      }
    }
}
