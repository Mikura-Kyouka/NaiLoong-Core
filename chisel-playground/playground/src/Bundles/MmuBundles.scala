package core
import chisel3._
import chisel3.util._
import MmuConfig._

class inst_addr_trans_datapacket extends Bundle {
    val trans_en = Input(Bool())
    val vaddr = Input(UInt(ADDR_WIDTH.W))
    val paddr = Output(UInt(ADDR_WIDTH.W))
}

class data_addr_trans_datapacket extends Bundle {
    val trans_en = Input(Bool())
    val vaddr = Input(UInt(ADDR_WIDTH.W))
    val paddr = Output(UInt(ADDR_WIDTH.W))
}

class csr_to_mmu_bundle extends Bundle {
    val tlbidx = new csr_tlbidx_bundle()
    val tlbehi = new csr_tlbehi_bundle()
    val tlbelo0 = new csr_tlbelo_bundle()
    val tlbelo1 = new csr_tlbelo_bundle()
    val asid = new csr_asid_bundle()
    val estat = new csr_estat_bundle()

    val crmd = new csr_crmd_bundle()
    val dmw0 = new csr_dmw_bundle()
    val dmw1 = new csr_dmw_bundle()
}

class mmu_to_csr_bundle extends Bundle {
    val tlb_hit = Bool()
    val tlbsrch_wen = Bool()
    val tlbrd_wen = Bool()
    val tlbidx = new csr_tlbidx_bundle()
    val tlbehi = new csr_tlbehi_bundle()
    val tlbelo0 = new csr_tlbelo_bundle()
    val tlbelo1 = new csr_tlbelo_bundle()
    val asid = new csr_asid_bundle()
}

class mmu_csr_datapacket extends Bundle {
    val csr_to_mmu = Input(new csr_to_mmu_bundle())
    val mmu_to_csr = Output(new mmu_to_csr_bundle())
}

class tlb_s_datapacket extends Bundle {
    // 其中输入的s_vppn来自访存虚地址的31..13 位，s_va_bit12 来自访存虚地址的12位，s_asid来自CSR.ASID的ASID 域。
    // TLB输出的s_ ppn和s_ ps用于产生最终的物理地址，s_found的结果用于判定是否产生TLB重填异常，s_found和s_v结果用于判定是否产生页无效异常，
    // s_found和s_ plv用于判定是否产生页特权等级不合规异常，s_found、s_v 和 s_d 结果用于判定是否产生页修改异常。
    val vppn = Input(UInt((ADDR_WIDTH - PAGE_WIDTH - 1).W))
    val va_bit12 = Input(UInt(1.W))
    val asid = Input(UInt(10.W))
    val found = Output(UInt(1.W))
    val index = Output(UInt(log2Ceil(TLB_NUM).W))
    val ppn = Output(UInt((ADDR_WIDTH - PAGE_WIDTH).W))
    val ps = Output(UInt(6.W))
    val plv = Output(UInt(2.W))
    val mat = Output(UInt(2.W))
    val d = Output(UInt(1.W))
    val v = Output(UInt(1.W))
}

class tlb_bundle extends Bundle {
    val vppn = UInt((ADDR_WIDTH - PAGE_WIDTH - 1).W)
    val ps = UInt(6.W)
    val g = UInt(1.W)
    val asid = UInt(10.W)
    val e = UInt(1.W)

    val ppn0 = UInt((ADDR_WIDTH - PAGE_WIDTH).W)
    val plv0 = UInt(2.W)
    val mat0 = UInt(2.W)
    val d0 = UInt(1.W)
    val v0 = UInt(1.W)

    val ppn1 = UInt((ADDR_WIDTH - PAGE_WIDTH).W)
    val plv1 = UInt(2.W)
    val mat1 = UInt(2.W)
    val d1 = UInt(1.W)
    val v1 = UInt(1.W)
}

class tlb_invalid_bundle extends Bundle {
    val valid = Bool()
    val op = UInt(5.W)
    val asid = UInt(10.W)
    val va = UInt((ADDR_WIDTH - PAGE_WIDTH - 1).W)
}