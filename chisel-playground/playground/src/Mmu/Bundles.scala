package core
import chisel3._
import chisel3.util._
import MmuConfig._

// 所有可能用到的信号都打包在一起，某个阶段没用的信号会被circt优化掉
class AddrTrans extends Bundle {
  val trans_en = Bool()
  val vaddr = UInt(32.W)
  val paddr = UInt(32.W)

  val hit_vec = Vec(TLB_NUM, Bool())
  val found = UInt(1.W)
  val index = UInt(log2Ceil(TLB_NUM).W)
  val ppn = UInt((ADDR_WIDTH - PAGE_WIDTH).W)
  val ps = UInt(6.W)
  val plv = UInt(2.W)
  val mat = UInt(2.W)
  val d = UInt(1.W)
  val v = UInt(1.W)
}

class Stage1Interface extends Bundle {
  val vppn = Output(UInt((ADDR_WIDTH - PAGE_WIDTH - 1).W))

  val hit_vec = Input(Vec(TLB_NUM, Bool()))
}

class Stage2Interface extends Bundle {
  val vppn = Output(UInt((ADDR_WIDTH - PAGE_WIDTH - 1).W))
  val va_bit12 = Output(UInt(1.W))
  val hit_vec = Output(Vec(TLB_NUM, Bool()))

  val found = Input(UInt(1.W))
  val index = Input(UInt(log2Ceil(TLB_NUM).W))
  val ppn = Input(UInt((ADDR_WIDTH - PAGE_WIDTH).W))
  val ps = Input(UInt(6.W))
  val plv = Input(UInt(2.W))
  val mat = Input(UInt(2.W))
  val d = Input(UInt(1.W))
  val v = Input(UInt(1.W))
}

class CsrToMmuBundle extends Bundle {
  val asid = Input(new csr_asid_bundle)
  val crmd = Input(new csr_crmd_bundle)
  val dmw0 = Input(new csr_dmw_bundle)
  val dmw1 = Input(new csr_dmw_bundle)
}

class TlbBundle extends Bundle {
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