package core
import chisel3._
import MmuConfig._

class MmuTop extends Module {
    val io = IO(new Bundle {
        //data
        val inst_addr_trans = new inst_addr_trans_datapacket()
        val data_addr_trans = new data_addr_trans_datapacket()
        val mmu_csr = new mmu_csr_datapacket()
        //control signal
    })

    // tlb
    val tlb = Module(new Tlb)

    // 指令和数据地址转换接口
    val inst_interface = Module(new TransInterface)
    val data_interface = Module(new TransInterface)

    inst_interface.io.vaddr := io.inst_addr_trans.vaddr
    io.inst_addr_trans.paddr := inst_interface.io.paddr
    inst_interface.io.crmd := io.mmu_csr.csr_to_mmu.crmd
    inst_interface.io.dmw0 := io.mmu_csr.csr_to_mmu.dmw0
    inst_interface.io.dmw1 := io.mmu_csr.csr_to_mmu.dmw1

    data_interface.io.vaddr := io.data_addr_trans.vaddr
    io.data_addr_trans.paddr := data_interface.io.paddr
    data_interface.io.crmd := io.mmu_csr.csr_to_mmu.crmd
    data_interface.io.dmw0 := io.mmu_csr.csr_to_mmu.dmw0
    data_interface.io.dmw1 := io.mmu_csr.csr_to_mmu.dmw1

    tlb.io.s0.vppn := io.inst_addr_trans.vaddr(ADDR_WIDTH - 1, PAGE_WIDTH + 1)
    tlb.io.s0.va_bit12 := io.inst_addr_trans.vaddr(PAGE_WIDTH)
    tlb.io.s0.asid := io.mmu_csr.csr_to_mmu.asid.asid
    inst_interface.io.ppn := tlb.io.s0.ppn

    tlb.io.s1.vppn := io.data_addr_trans.vaddr(ADDR_WIDTH - 1, PAGE_WIDTH + 1)
    tlb.io.s1.va_bit12 := io.data_addr_trans.vaddr(PAGE_WIDTH)
    tlb.io.s1.asid := io.mmu_csr.csr_to_mmu.asid.asid
    data_interface.io.ppn := tlb.io.s1.ppn

    // tlb control instructions
    // tlbsrch
    io.mmu_csr.mmu_to_csr.tlb_hit := tlb.io.s1.found
    io.mmu_csr.mmu_to_csr.tlbidx.idx := tlb.io.s1.index 
    io.mmu_csr.mmu_to_csr.tlbsrch_wen := DontCare // TODO: control signals

    // tlbrd
    tlb.io.r_index := io.mmu_csr.csr_to_mmu.tlbidx.idx

    io.mmu_csr.mmu_to_csr.tlbrd_wen := DontCare // TODO: control signals

    io.mmu_csr.mmu_to_csr.tlbehi.vppn := tlb.io.r.vppn

    io.mmu_csr.mmu_to_csr.tlbelo0.ppn := tlb.io.r.ppn0
    io.mmu_csr.mmu_to_csr.tlbelo0.plv := tlb.io.r.plv0
    io.mmu_csr.mmu_to_csr.tlbelo0.mat := tlb.io.r.mat0
    io.mmu_csr.mmu_to_csr.tlbelo0.d := tlb.io.r.d0
    io.mmu_csr.mmu_to_csr.tlbelo0.v := tlb.io.r.v0
    io.mmu_csr.mmu_to_csr.tlbelo0.g := tlb.io.r.g

    io.mmu_csr.mmu_to_csr.tlbelo1.ppn := tlb.io.r.ppn1
    io.mmu_csr.mmu_to_csr.tlbelo1.plv := tlb.io.r.plv1
    io.mmu_csr.mmu_to_csr.tlbelo1.mat := tlb.io.r.mat1
    io.mmu_csr.mmu_to_csr.tlbelo1.d := tlb.io.r.d1
    io.mmu_csr.mmu_to_csr.tlbelo1.v := tlb.io.r.v1
    io.mmu_csr.mmu_to_csr.tlbelo1.g := tlb.io.r.g

    io.mmu_csr.mmu_to_csr.tlbidx.ps := tlb.io.r.ps

    // tlbwr/tlbfill
    // 仅实现tlbwr，需要控制信号
    tlb.io.w_en := DontCare // TODO: control signals
    tlb.io.w_index := io.mmu_csr.csr_to_mmu.tlbidx.idx

    tlb.io.w.vppn := io.mmu_csr.csr_to_mmu.tlbehi.vppn
    tlb.io.w.ps := io.mmu_csr.csr_to_mmu.tlbidx.ps
    tlb.io.w.g := io.mmu_csr.csr_to_mmu.tlbelo0.g
    tlb.io.w.asid := io.mmu_csr.csr_to_mmu.asid.asid
    tlb.io.w.e := (io.mmu_csr.csr_to_mmu.estat.ecode === "h3F".U) || ~io.mmu_csr.csr_to_mmu.tlbidx.ne.asBool

    tlb.io.w.ppn0 := io.mmu_csr.csr_to_mmu.tlbelo0.ppn
    tlb.io.w.plv0 := io.mmu_csr.csr_to_mmu.tlbelo0.plv
    tlb.io.w.mat0 := io.mmu_csr.csr_to_mmu.tlbelo0.mat
    tlb.io.w.d0 := io.mmu_csr.csr_to_mmu.tlbelo0.d
    tlb.io.w.v0 := io.mmu_csr.csr_to_mmu.tlbelo0.v

    tlb.io.w.ppn1 := io.mmu_csr.csr_to_mmu.tlbelo1.ppn
    tlb.io.w.plv1 := io.mmu_csr.csr_to_mmu.tlbelo1.plv
    tlb.io.w.mat1 := io.mmu_csr.csr_to_mmu.tlbelo1.mat
    tlb.io.w.d1 := io.mmu_csr.csr_to_mmu.tlbelo1.d
    tlb.io.w.v1 := io.mmu_csr.csr_to_mmu.tlbelo1.v

    // invtlb
    tlb.io.invtlb.valid := DontCare
    tlb.io.invtlb.op := DontCare
    tlb.io.invtlb.asid := DontCare
    tlb.io.invtlb.va := DontCare

    // dontcare
    io.mmu_csr.mmu_to_csr.tlbidx.ne := DontCare
    io.mmu_csr.mmu_to_csr.tlbidx.zero30 := DontCare
    io.mmu_csr.mmu_to_csr.tlbidx.zero23_16 := DontCare
    io.mmu_csr.mmu_to_csr.tlbehi.zero12_0 := DontCare
    io.mmu_csr.mmu_to_csr.tlbelo0.zero7 := DontCare
    io.mmu_csr.mmu_to_csr.tlbelo1.zero7 := DontCare
    io.mmu_csr.mmu_to_csr.asid.zero31_24 := DontCare
    io.mmu_csr.mmu_to_csr.asid.asidbits := DontCare
    io.mmu_csr.mmu_to_csr.asid.zero15_10 := DontCare
    io.mmu_csr.mmu_to_csr.asid.asid := DontCare
  
}

object GenMmu extends App {
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
    circt.stage.ChiselStage.emitSystemVerilogFile(new MmuTop(), args, firtoolOptions)
}
