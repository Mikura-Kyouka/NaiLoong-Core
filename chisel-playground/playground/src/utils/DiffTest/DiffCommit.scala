package core
import chisel3._
import chisel3.util._

class DiffCommit extends Module {
  val io = IO(new Bundle {
    // DifftestInstrCommit
    val instr = Input(Vec(4, new DiffInstrBundle()))

    // DifftestExcpEvent
    val excp = Input(new DiffExcpBundle())

    // DifftestTrapEvent
    // 空

    // DifftestStoreEvent
    val store = Input(new DiffStoreBundle())

    // DifftestLoadEvent

    // DifftestCSRRegState

    val reg = Input(Vec(32, UInt(32.W)))
  })

  val timer64 = RegInit(0.U(64.W))
  timer64 := timer64 + 1.U

  val DiffBridge = Module(new DiffBridge())
  DiffBridge.io.clock := clock
  DiffBridge.io.coreid := 0.U

  DiffBridge.io.index_0 := 0.U
  DiffBridge.io.Instrvalid_0 := RegNext(io.instr(0).valid, 0.U)
  DiffBridge.io.the_pc_0 := RegNext(io.instr(0).pc, 0.U)
  DiffBridge.io.instr_0 := RegNext(io.instr(0).instr, 0.U)
  DiffBridge.io.skip_0 := 0.U
  DiffBridge.io.is_TLBFILL_0 := RegNext(io.instr(0).is_TLBFILL, 0.U)
  DiffBridge.io.TLBFILL_index_0 := RegNext(io.instr(0).TLBFILL_index, 0.U)
  DiffBridge.io.is_CNTinst_0 := RegNext(io.instr(0).is_CNTinst, 0.U)
  // DiffBridge.io.timer_64_value_0 := RegNext(io.instr(0).timer_64_value, 0.U)
  DiffBridge.io.timer_64_value_0 := timer64
  DiffBridge.io.wen_0 := RegNext(io.instr(0).wen, 0.U)
  DiffBridge.io.wdest_0 := RegNext(io.instr(0).wdest, 0.U)
  DiffBridge.io.wdata_0 := RegNext(io.instr(0).wdata, 0.U)
  DiffBridge.io.csr_rstat_0 := RegNext(io.instr(0).csr_rstat, 0.U)
  DiffBridge.io.csr_data_0 := RegNext(io.instr(0).csr_data, 0.U)

  DiffBridge.io.index_1 := 1.U
  DiffBridge.io.Instrvalid_1 := RegNext(io.instr(1).valid, 0.U)
  DiffBridge.io.the_pc_1 := RegNext(io.instr(1).pc, 0.U)
  DiffBridge.io.instr_1 := RegNext(io.instr(1).instr, 0.U)
  DiffBridge.io.skip_1 := 0.U
  DiffBridge.io.is_TLBFILL_1 := RegNext(io.instr(1).is_TLBFILL, 0.U)
  DiffBridge.io.TLBFILL_index_1 := RegNext(io.instr(1).TLBFILL_index, 0.U)
  DiffBridge.io.is_CNTinst_1 := RegNext(io.instr(1).is_CNTinst, 0.U)
  // DiffBridge.io.timer_64_value_1 := RegNext(io.instr(1).timer_64_value, 0.U)
  DiffBridge.io.timer_64_value_1 := timer64
  DiffBridge.io.wen_1 := RegNext(io.instr(1).wen, 0.U)
  DiffBridge.io.wdest_1 := RegNext(io.instr(1).wdest, 0.U)
  DiffBridge.io.wdata_1 := RegNext(io.instr(1).wdata, 0.U)
  DiffBridge.io.csr_rstat_1 := RegNext(io.instr(1).csr_rstat, 0.U)
  DiffBridge.io.csr_data_1 := RegNext(io.instr(1).csr_data, 0.U)

  DiffBridge.io.index_2 := 2.U
  DiffBridge.io.Instrvalid_2 := RegNext(io.instr(2).valid, 0.U)
  DiffBridge.io.the_pc_2 := RegNext(io.instr(2).pc, 0.U)
  DiffBridge.io.instr_2 := RegNext(io.instr(2).instr, 0.U)
  DiffBridge.io.skip_2 := 0.U
  DiffBridge.io.is_TLBFILL_2 := RegNext(io.instr(2).is_TLBFILL, 0.U)
  DiffBridge.io.TLBFILL_index_2 := RegNext(io.instr(2).TLBFILL_index, 0.U)
  DiffBridge.io.is_CNTinst_2 := RegNext(io.instr(2).is_CNTinst, 0.U)
  DiffBridge.io.timer_64_value_2 := RegNext(io.instr(2).timer_64_value, 0.U)
  DiffBridge.io.wen_2 := RegNext(io.instr(2).wen, 0.U)
  DiffBridge.io.wdest_2 := RegNext(io.instr(2).wdest, 0.U)
  DiffBridge.io.wdata_2 := RegNext(io.instr(2).wdata, 0.U)
  DiffBridge.io.csr_rstat_2 := RegNext(io.instr(2).csr_rstat, 0.U)
  DiffBridge.io.csr_data_2 := RegNext(io.instr(2).csr_data, 0.U)

  DiffBridge.io.index_3 := 3.U
  DiffBridge.io.Instrvalid_3 := RegNext(io.instr(3).valid, 0.U)
  DiffBridge.io.the_pc_3 := RegNext(io.instr(3).pc, 0.U)
  DiffBridge.io.instr_3 := RegNext(io.instr(3).instr, 0.U)
  DiffBridge.io.skip_3 := 0.U
  DiffBridge.io.is_TLBFILL_3 := RegNext(io.instr(3).is_TLBFILL, 0.U)
  DiffBridge.io.TLBFILL_index_3 := RegNext(io.instr(3).TLBFILL_index, 0.U)
  DiffBridge.io.is_CNTinst_3 := RegNext(io.instr(3).is_CNTinst, 0.U)
  DiffBridge.io.timer_64_value_3 := RegNext(io.instr(3).timer_64_value, 0.U)
  DiffBridge.io.wen_3 := RegNext(io.instr(3).wen, 0.U)
  DiffBridge.io.wdest_3 := RegNext(io.instr(3).wdest, 0.U)
  DiffBridge.io.wdata_3 := RegNext(io.instr(3).wdata, 0.U)
  DiffBridge.io.csr_rstat_3 := RegNext(io.instr(3).csr_rstat, 0.U)
  DiffBridge.io.csr_data_3 := RegNext(io.instr(3).csr_data, 0.U)

  DiffBridge.io.excp_valid := RegNext(io.excp.excp_valid, 0.U)
  DiffBridge.io.eret := RegNext(io.excp.eret, 0.U)
  DiffBridge.io.intrNo := RegNext(io.excp.intrNo, 0.U)
  DiffBridge.io.cause := RegNext(io.excp.cause, 0.U)
  DiffBridge.io.exceptionPC := RegNext(io.excp.exceptionPC, 0.U)
  DiffBridge.io.exceptionInst := RegNext(io.excp.exceptionInst, 0.U)

  DiffBridge.io.storeValid := RegNext(io.store.valid, 0.U)
  DiffBridge.io.storeIndex := 0.U
  DiffBridge.io.storePaddr := RegNext(io.store.paddr, 0.U)
  DiffBridge.io.storeVaddr := RegNext(io.store.vaddr, 0.U)
  DiffBridge.io.storeData := RegNext(io.store.data, 0.U)

  DiffBridge.io.REG := io.reg
}

class DiffBridge extends BlackBox with HasBlackBoxPath {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val coreid = Input(UInt(8.W))

    val index_0 = Input(UInt(8.W))
    val Instrvalid_0 = Input(Bool())
    val the_pc_0 = Input(UInt(64.W))
    val instr_0 = Input(UInt(32.W))
    val skip_0 = Input(Bool())
    val is_TLBFILL_0 = Input(Bool())
    val TLBFILL_index_0 = Input(UInt(5.W))
    val is_CNTinst_0 = Input(Bool())
    val timer_64_value_0 = Input(UInt(64.W))
    val wen_0 = Input(Bool())
    val wdest_0 = Input(UInt(8.W))
    val wdata_0 = Input(UInt(64.W))
    val csr_rstat_0 = Input(Bool())          
    val csr_data_0 = Input(UInt(32.W))       

    val index_1 = Input(UInt(8.W))
    val Instrvalid_1 = Input(Bool())
    val the_pc_1 = Input(UInt(64.W))
    val instr_1 = Input(UInt(32.W))
    val skip_1 = Input(Bool())
    val is_TLBFILL_1 = Input(Bool())
    val TLBFILL_index_1 = Input(UInt(5.W))
    val is_CNTinst_1 = Input(Bool())
    val timer_64_value_1 = Input(UInt(64.W))
    val wen_1 = Input(Bool())
    val wdest_1 = Input(UInt(8.W))
    val wdata_1 = Input(UInt(64.W))
    val csr_rstat_1 = Input(Bool())          
    val csr_data_1 = Input(UInt(32.W))

    val index_2 = Input(UInt(8.W))
    val Instrvalid_2 = Input(Bool())
    val the_pc_2 = Input(UInt(64.W))
    val instr_2 = Input(UInt(32.W))
    val skip_2 = Input(Bool())
    val is_TLBFILL_2 = Input(Bool())
    val TLBFILL_index_2 = Input(UInt(5.W))
    val is_CNTinst_2 = Input(Bool())
    val timer_64_value_2 = Input(UInt(64.W))
    val wen_2 = Input(Bool())
    val wdest_2 = Input(UInt(8.W))
    val wdata_2 = Input(UInt(64.W))
    val csr_rstat_2 = Input(Bool())
    val csr_data_2 = Input(UInt(32.W))
    
    val index_3 = Input(UInt(8.W))
    val Instrvalid_3 = Input(Bool())
    val the_pc_3 = Input(UInt(64.W))
    val instr_3 = Input(UInt(32.W))
    val skip_3 = Input(Bool())
    val is_TLBFILL_3 = Input(Bool())
    val TLBFILL_index_3 = Input(UInt(5.W))
    val is_CNTinst_3 = Input(Bool())
    val timer_64_value_3 = Input(UInt(64.W))
    val wen_3 = Input(Bool())
    val wdest_3 = Input(UInt(8.W))
    val wdata_3 = Input(UInt(64.W))
    val csr_rstat_3 = Input(Bool())
    val csr_data_3 = Input(UInt(32.W))

    val excp_valid = Input(Bool())
    val eret = Input(Bool())
    val intrNo = Input(UInt(11.W))
    val cause = Input(UInt(6.W))
    val exceptionPC = Input(UInt(32.W))
    val exceptionInst =Input(UInt(32.W))

    //DifftestStoreEvent
    val storeIndex = Input(UInt(8.W))
    val storeValid = Input(UInt(8.W))
    val storePaddr = Input(UInt(64.W))
    val storeVaddr = Input(UInt(64.W))
    val storeData = Input(UInt(64.W))

    val REG = Input(Vec(32, UInt(64.W)))
  })
}