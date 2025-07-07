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

  // val If = Module(new TempIf)
  val If = Module(new IFU)
  val Id = Module(new IDU)
  val Rn = Module(new Rename)
  val Dispatch = Module(new Dispatch)
  val Issue = Module(new IssueTop)
  val Ex = Module(new Execute)

  val rob = Module(new Rob)

  val arb = Module(new Arb)

  val csr = Module(new CSR)

  val bpu = Module(new BPU)

  val mmu = Module(new MMU)
  

  val nextPC = Cat(If.io.nextPC(31, 4), 0.U(4.W))
  bpu.io.pc(0) := nextPC
  bpu.io.pc(1) := nextPC + 4.U
  bpu.io.pc(2) := nextPC + 8.U
  bpu.io.pc(3) := nextPC + 12.U

  val offset = RegNext(If.io.nextPC(3, 2)) // 4 bytes aligned

  If.io.BrPredictTaken := DontCare
  If.io.BrPredictTaken(0).predictTaken := bpu.io.taken(0) && offset <= 0.U
  If.io.BrPredictTaken(0).predictTarget := bpu.io.target(0)
  If.io.BrPredictTaken(1).predictTaken := bpu.io.taken(1) && offset <= 1.U
  If.io.BrPredictTaken(1).predictTarget := bpu.io.target(1)
  If.io.BrPredictTaken(2).predictTaken := bpu.io.taken(2) && offset <= 2.U
  If.io.BrPredictTaken(2).predictTarget := bpu.io.target(2)
  If.io.BrPredictTaken(3).predictTaken := bpu.io.taken(3) && offset <= 3.U
  If.io.BrPredictTaken(3).predictTarget := bpu.io.target(3)

  val pipedBrMisPredInfo = RegNext(rob.io.brMisPredInfo)
  bpu.io.train.pc := pipedBrMisPredInfo.brMisPredPC
  bpu.io.train.target := pipedBrMisPredInfo.brMisPredTarget
  bpu.io.train.taken := pipedBrMisPredInfo.actuallyTaken
  bpu.io.train.valid := pipedBrMisPredInfo.brMisPred.valid

  dontTouch(bpu.io)

  val flush = rob.io.flush

  PipelineConnect(If.io.out, Id.io.in, Id.io.out.fire, flush)
  PipelineConnect(Id.io.out, Rn.io.in, Rn.io.out.fire, flush)
  PipelineConnect(Rn.io.out, Dispatch.io.in, Dispatch.io.out.map(_.fire).reduce(_ || _), flush)
  PipelineConnect2(Dispatch.io.out(0), Issue.io.in(0), Issue.io.out(0).fire, flush)
  PipelineConnect2(Dispatch.io.out(1), Issue.io.in(1), Issue.io.out(1).fire, flush)
  PipelineConnect2(Dispatch.io.out(2), Issue.io.in(2), Issue.io.out(2).fire, flush)
  PipelineConnect2(Dispatch.io.out(3), Issue.io.in(3), Issue.io.out(3).fire, flush)
  PipelineConnect2(Dispatch.io.out(4), Issue.io.in(4), Issue.io.out(4).fire, flush)
  PipelineConnect(Issue.io.out(0), Ex.io.in(0), Ex.io.out(0).fire, flush)
  PipelineConnect(Issue.io.out(1), Ex.io.in(1), Ex.io.out(1).fire, flush)
  PipelineConnect(Issue.io.out(2), Ex.io.in(2), Ex.io.out(2).fire, flush)
  PipelineConnect(Issue.io.out(3), Ex.io.in(3), Ex.io.out(3).fire, flush)
  PipelineConnect(Issue.io.out(4), Ex.io.in(4), Ex.io.out(4).fire, flush)

  val ifAXI = Wire(new AXI)

  If.io.intrpt := io.intrpt

  ifAXI.arid := If.io.axi.arid
  ifAXI.araddr := If.io.axi.araddr
  ifAXI.arlen := If.io.axi.arlen
  ifAXI.arsize := If.io.axi.arsize
  ifAXI.arburst := If.io.axi.arburst
  io.arlock  := DontCare
  io.arcache := DontCare
  io.arprot  := DontCare
  ifAXI.arvalid := If.io.axi.arvalid
  If.io.axi.arready := ifAXI.arready
  ifAXI.arvalid := If.io.axi.arvalid
  If.io.axi.arready := ifAXI.arready

  If.io.axi.rid := ifAXI.rid
  If.io.axi.rdata := ifAXI.rdata
  If.io.axi.rresp := ifAXI.rresp
  If.io.axi.rlast := ifAXI.rlast
  If.io.axi.rvalid := ifAXI.rvalid
  ifAXI.rready := If.io.axi.rready
  If.io.axi.rid := ifAXI.rid
  If.io.axi.rdata := ifAXI.rdata
  If.io.axi.rresp := ifAXI.rresp
  If.io.axi.rlast := ifAXI.rlast
  If.io.axi.rvalid := ifAXI.rvalid
  ifAXI.rready := If.io.axi.rready

  ifAXI.awid := If.io.axi.awid
  ifAXI.awaddr := If.io.axi.awaddr
  ifAXI.awlen := If.io.axi.awlen
  ifAXI.awsize := If.io.axi.awsize
  ifAXI.awburst := If.io.axi.awburst
  ifAXI.awid := If.io.axi.awid
  ifAXI.awaddr := If.io.axi.awaddr
  ifAXI.awlen := If.io.axi.awlen
  ifAXI.awsize := If.io.axi.awsize
  ifAXI.awburst := If.io.axi.awburst
  io.awlock  := DontCare
  io.awcache := DontCare
  io.awprot  := DontCare
  ifAXI.awvalid := If.io.axi.awvalid
  If.io.axi.awready := ifAXI.awready

  io.wid := DontCare
  ifAXI.wdata := If.io.axi.wdata
  ifAXI.wstrb := If.io.axi.wstrb
  ifAXI.wlast := If.io.axi.wlast
  ifAXI.wvalid := If.io.axi.wvalid
  ifAXI.wid := If.io.axi.wid
  If.io.axi.wready := ifAXI.wready

  If.io.axi.bid := ifAXI.bid
  If.io.axi.bresp := ifAXI.bresp
  If.io.axi.bvalid := ifAXI.bvalid
  ifAXI.bready := If.io.axi.bready

  If.io.break_point := io.break_point
  If.io.infor_flag := io.infor_flag
  If.io.reg_num := io.reg_num
  io.ws_valid := If.io.ws_valid
  io.rf_rdata := If.io.rf_rdata

  val lsAXI = Wire(new AXI)
  lsAXI <> Ex.io.lsAXI

  arb.io.ifu <> ifAXI
  arb.io.lsu <> lsAXI

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

  io.wid := arb.io.out.wid
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
    trace.rf_we    := Mux(item.bits.preg =/= 0.U && !item.bits.excp, "b1111".U(4.W), 0.U(4.W))
    trace.rf_wnum  := item.bits.dest
    trace.rf_wdata := item.bits.data
    trace.valid    := item.valid
    trace.seq_num  := DontCare
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

  If.io.flush := flush
  If.io.dnpc := rob.io.newPC
  If.io.pcSel := flush
  
  dontTouch(Rn.io.robAllocate)

  // Rn.io.out.ready := true.B
  // dontTouch(Rn.io.out)
  // dontTouch(Issue.io.out)
  // Issue.io.out(0).ready := true.B
  // Issue.io.out(1).ready := true.B
  // Issue.io.out(2).ready := true.B
  // Issue.io.out(3).ready := true.B
  // Issue.io.out(4).ready := true.B

  Issue.io.rtrInstr := DontCare
  
  for(i <- 0 until 5) {
    Issue.io.ex_bypass(i).valid := Ex.io.out(i).valid
    Issue.io.ex_bypass(i).dest := Ex.io.out(i).bits.preg
    Issue.io.ex_bypass(i).data := Ex.io.out(i).bits.data
  }

  for(i <- 0 until RobConfig.ROB_CMT_NUM) {
    Issue.io.rtrInstr(i).bits.data := rob.io.commit.commit(i).bits.data
    Issue.io.rtrInstr(i).valid := rob.io.commitInstr(i).valid
    Issue.io.rtrInstr(i).bits.preg := rob.io.commit.commit(i).bits.preg
  }

  Issue.io.flush := flush
  
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
    rob.io.writeback(i).bits.redirect := Ex.io.out(i).bits.redirect
    rob.io.writeback(i).bits.csrNewData := Ex.io.out(i).bits.csrNewData
    rob.io.writeback(i).bits.exceptionVec := Ex.io.out(i).bits.exceptionVec
    // for load/store difftest
    rob.io.writeback(i).bits.paddr := Ex.io.out(i).bits.paddr
    rob.io.writeback(i).bits.wdata := Ex.io.out(i).bits.wdata
    rob.io.writeback(i).bits.optype := Ex.io.out(i).bits.optype
    rob.io.writeback(i).bits.fuType := Ex.io.out(i).bits.fuType
    rob.io.writeback(i).bits.timer64 := Ex.io.out(i).bits.timer64
  }

  // allocate rob entries in rename stage
  Rn.io.robAllocate <> rob.io.allocate

  // lsu <=> rob
  Ex.io.RobLsuIn <> rob.io.RobLsuOut
  Ex.io.RobLsuOut <> rob.io.RobLsuIn
  Ex.io.flush := flush

  // arf and rat update
  Rn.io.rob <> rob.io.commit

  // branch handle logic
  Rn.io.brMispredict := rob.io.brMisPredInfo
  Rn.io.exception := rob.io.exceptionInfo.valid

  Rn.io.flush := flush

  // rob <=> csr
  csr.io.write <> rob.io.commitCSR
  csr.io.exceptionInfo <> rob.io.exceptionInfo
  // ex <=> csr
  csr.io.read <> Ex.io.csrRead
  Ex.io.markIntrpt := csr.io.markIntrpt
  // mmu <=> csr
  csr.io.from_mmu <> mmu.io.to_csr
  csr.io.to_mmu <> mmu.io.from_csr

  // FIXME: mmu io is empty!
  mmu.io.in0 := DontCare
  mmu.io.in1 := DontCare
  mmu.io.flush := DontCare
  mmu.io.w := DontCare
  mmu.io.wen := DontCare
  mmu.io.w_index := DontCare
  mmu.io.r_index := DontCare
  mmu.io.tlb_inst := DontCare
  mmu.io.out0.ready := true.B
  mmu.io.out1.ready := true.B

  rob.io.plv := csr.io.plv

  if (GenCtrl.USE_DIFF) {
    val DiffCommit = Module(new DiffCommit)

    // 1) 收集原始 4 路信号
    val diffValids = VecInit((0 until RobConfig.ROB_CMT_NUM).map { i =>
      rob.io.commitInstr(i).valid && rob.io.commit.commit(i).bits.inst_valid
    })
    val diffPCs       = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commitPC(i).bits))
    val diffInsts     = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commitInstr(i).bits))
    val diffDests     = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commit.commit(i).bits.dest))
    val diffDatas     = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commit.commit(i).bits.data))
    val diffcsr_rstat = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commit.commit(i).bits.csr_rstat))
    val diffcsr_data  = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commit.commit(i).bits.csr_data))
    val diffExcp      = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commit.commit(i).bits.excp))
    val diffTimer64   = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commit.commit(i).bits.timer64))
    val diffIsCNTinst = VecInit((0 until RobConfig.ROB_CMT_NUM).map(i => rob.io.commitInstr(i).bits(31, 11) === "b000000000000000001100".U))
    val diffWens      = diffDests.map(_ =/= 0.U)

    // 2) 计算 prefixSum：prefixSum(j) = 前 j 路中有多少 valid
    //    prefixSum(0)=0, prefixSum(1)=valid(0), prefixSum(2)=valid(0)+valid(1), ...
    val prefixSum = Wire(Vec(RobConfig.ROB_CMT_NUM, UInt(3.W)))
    prefixSum(0) := 0.U
    for (j <- 1 until RobConfig.ROB_CMT_NUM) {
      prefixSum(j) := prefixSum(j-1) + diffValids(j-1).asUInt
    }

    // 3) 对每一路输出 k，选出原始中第 k 个 valid 为真的输入
    for (k <- 0 until RobConfig.ROB_CMT_NUM) {
      // sel(j) = “第 j 路输入 valid，并且在它之前正好有 k 条 valid”
      val sel = VecInit((0 until RobConfig.ROB_CMT_NUM).map { j =>
        diffValids(j) && (prefixSum(j) === k.U)
      })
      // 输出 valid
      DiffCommit.io.instr(k).valid := sel.asUInt.orR && !Mux1H(sel.zip(diffExcp))

      // 用 Mux1H 实现 one-hot 选择
      DiffCommit.io.instr(k).pc    := Mux1H(sel.zip(diffPCs))
      DiffCommit.io.instr(k).instr := Mux1H(sel.zip(diffInsts))
      DiffCommit.io.instr(k).wdest := Mux1H(sel.zip(diffDests))
      DiffCommit.io.instr(k).wdata := Mux1H(sel.zip(diffDatas))
      DiffCommit.io.instr(k).wen   := Mux1H(sel.zip(diffWens))

      DiffCommit.io.instr(k).skip          := DontCare
      DiffCommit.io.instr(k).is_TLBFILL    := DontCare
      DiffCommit.io.instr(k).TLBFILL_index := DontCare
      DiffCommit.io.instr(k).is_CNTinst    := Mux1H(sel.zip(diffIsCNTinst))
      DiffCommit.io.instr(k).timer_64_value:= Mux1H(sel.zip(diffTimer64))
      DiffCommit.io.instr(k).csr_rstat     := Mux1H(sel.zip(diffcsr_rstat))
      DiffCommit.io.instr(k).csr_data      := Mux1H(sel.zip(diffcsr_data))

      DiffCommit.io.excp.excp_valid := rob.io.exceptionInfo.valid
      DiffCommit.io.excp.eret := csr.io.exceptionInfo.eret
      DiffCommit.io.excp.intrNo := csr.io.exceptionInfo.intrNo
      DiffCommit.io.excp.cause := csr.io.exceptionInfo.cause
      DiffCommit.io.excp.exceptionPC := rob.io.exceptionInfo.exceptionPC
      DiffCommit.io.excp.exceptionInst := rob.io.exceptionInfo.exceptionInst
    }

    // 4) 其余接口
    DiffCommit.io.reg := Rn.io.arf
    DiffCommit.io.csr := csr.io.difftest

    val isSt = rob.io.commitLS.map { commit =>
      commit.valid && (commit.bits.optype === LSUOpType.sw ||
                       commit.bits.optype === LSUOpType.sh ||
                       commit.bits.optype === LSUOpType.sb)
    }
    val isValidSt = isSt.zip(diffExcp).map { case (st, excp) => st && !excp }
    val stInfo = Wire(new LSCommitInfo)
    when (isValidSt(0)) {
      stInfo := rob.io.commitLS(0).bits
    }.elsewhen (isValidSt(1)) {
      stInfo := rob.io.commitLS(1).bits
    }.elsewhen (isValidSt(2)) {
      stInfo := rob.io.commitLS(2).bits
    }.otherwise {
      stInfo := 0.U.asTypeOf(new LSCommitInfo)
    }
    val storeType = MuxLookup(stInfo.optype, 0.U)(
      List(
        LSUOpType.sw -> "b00000100".U,
        LSUOpType.sh -> "b00000010".U,
        LSUOpType.sb -> "b00000001".U,
      )
    )

    DiffCommit.io.store.valid := Mux(isValidSt.reduce(_ || _), storeType, 0.U)
    DiffCommit.io.store.paddr := stInfo.paddr
    DiffCommit.io.store.vaddr := stInfo.paddr
    DiffCommit.io.store.data  := stInfo.wdata
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