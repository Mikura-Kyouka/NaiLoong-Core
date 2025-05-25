package core
import utils._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import core.FuType.csr


object CSROpType {
  def jmp  = "b000".U
  def wrt  = "b001".U
  def set  = "b010".U
  def clr  = "b011".U
  def wrti = "b101".U
  def seti = "b110".U
  def clri = "b111".U
}

object CsrName {
  val CRMD  =0.U(14.W)
  val PRMD  =1.U(14.W)
  val ECFG  =4.U(14.W)
  val ESTAT =5.U(14.W)
  val ERA   =6.U(14.W)
  val BADV  =7.U(14.W)
  val EENTRY=12.U(14.W)
  val TLBIDX=16.U(14.W)
  val TLBEHI=17.U(14.W)
  val TLBELO0=18.U(14.W)
  val TLBELO1=19.U(14.W)
  val ASID  =24.U(14.W)
  val PGDL  =25.U(14.W)
  val PGDH  =26.U(14.W)
  val PGD   =27.U(14.W)
  val SAVE0 =48.U(14.W)
  val SAVE1 =49.U(14.W)
  val SAVE2 =50.U(14.W)
  val SAVE3 =51.U(14.W)
  val TID   =64.U(14.W)
  val TCFG  =65.U(14.W)
  val TVAL  =66.U(14.W)
  val CNTC  =67.U(14.W)
  val TICLR =68.U(14.W)
  val LLBCTL=96.U(14.W)
  val TLBRENTRY=136.U(14.W)
  val DMW0  =384.U(14.W)
  val DMW1  =385.U(14.W)
}

class DiffCSRBundle extends Bundle {
  val csr_crmd = UInt(32.W)
  val csr_prmd = UInt(32.W)
  val csr_ecfg = UInt(32.W)
  val csr_estat = UInt(32.W)
  val csr_era = UInt(32.W)
  val csr_badv = UInt(32.W)
  val csr_eentry = UInt(32.W)
  val csr_tlbidx = UInt(32.W)
  val csr_tlbehi = UInt(32.W)
  val csr_tlbel0 = UInt(32.W)
  val csr_tlbel1 = UInt(32.W)
  val csr_asid = UInt(32.W)
  val csr_pgdl = UInt(32.W)
  val csr_pgdh = UInt(32.W)
  val csr_pgd = UInt(32.W)
  val csr_save0 = UInt(32.W)
  val csr_save1 = UInt(32.W)
  val csr_save2 = UInt(32.W)
  val csr_save3 = UInt(32.W)
  val csr_tid = UInt(32.W)
  val csr_tcfg = UInt(32.W)
  val csr_tval = UInt(32.W)
  val csr_cntc = UInt(32.W)
  val csr_ticlr = UInt(32.W)
  val csr_llbctl = UInt(32.W)
  val csr_tlbrentry = UInt(32.W)
  val csr_dmw0 = UInt(32.W)
  val csr_dmw1 = UInt(32.W)
}

class CSRIO extends FunctionUnitIO {

}

class CSR extends Module {
    // val io = IO (new CSRIO)
    // val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)

  val io = IO(new Bundle {
    val read = Vec(2, new csr_read_bundle)
    val write = Flipped(Vec(4, Valid(new csr_write_bundle)))
    val exceptionInfo = new csr_excp_bundle
    val difftest = Output(new DiffCSRBundle)
  })
  val csr_crmd = RegInit(0.U.asTypeOf(new csr_crmd_bundle))
  val csr_prmd = RegInit(0.U.asTypeOf(new csr_prmd_bundle))
  val csr_ecfg = RegInit(0.U.asTypeOf(new csr_ecfg_bundle))
  val csr_estat = RegInit(0.U.asTypeOf(new csr_estat_bundle))
  val csr_era = RegInit(0.U(32.W))
  val csr_badv = RegInit(0.U(32.W))
  val csr_eentry = RegInit(0.U.asTypeOf(new csr_eentry_bundle))
  val csr_tlbidx = RegInit(0.U.asTypeOf(new csr_tlbidx_bundle))
  val csr_tlbehi = RegInit(0.U.asTypeOf(new csr_tlbehi_bundle))
  val csr_tlbel0 = RegInit(0.U.asTypeOf(new csr_tlbelo_bundle))
  val csr_tlbel1 = RegInit(0.U.asTypeOf(new csr_tlbelo_bundle))
  val csr_asid = RegInit(0.U.asTypeOf(new csr_asid_bundle))
  val csr_pgdl = RegInit(0.U.asTypeOf(new csr_pgdx_bundle))
  val csr_pgdh = RegInit(0.U.asTypeOf(new csr_pgdx_bundle))
  val csr_pgd = RegInit(0.U.asTypeOf(new csr_pgdx_bundle))
  val csr_save0 = RegInit(0.U(32.W))
  val csr_save1 = RegInit(0.U(32.W))
  val csr_save2 = RegInit(0.U(32.W))
  val csr_save3 = RegInit(0.U(32.W))
  val csr_tid = RegInit(0.U(32.W))
  val csr_tcfg = RegInit(0.U.asTypeOf(new csr_tcfg_bundle))
  val csr_tval = RegInit(0.U(32.W))
  val csr_cntc = RegInit(0.U(32.W))
  val csr_ticlr = RegInit(0.U(32.W))
  val csr_llbctl = RegInit(0.U.asTypeOf(new csr_llbctl_bundle))
  val csr_tlbrentry = RegInit(0.U.asTypeOf(new csr_tlbrentry_bundle))
  val csr_dmw0 = RegInit(0.U.asTypeOf(new csr_dmw_bundle))
  val csr_dmw1 = RegInit(0.U.asTypeOf(new csr_dmw_bundle))
  val timer64 = RegInit(0.U(64.W))
  timer64 := timer64 + 1.U
  val debug_csr_crmd = csr_crmd.asUInt
  val debug_csr_prmd = csr_prmd.asUInt
  val debug_csr_ecfg = csr_ecfg.asUInt
  val debug_csr_estat = csr_estat.asUInt
  val debug_csr_era = csr_era.asUInt
  val debug_csr_badv = csr_badv.asUInt
  val debug_csr_eentry = csr_eentry.asUInt
  val debug_csr_tlbidx = csr_tlbidx.asUInt
  val debug_csr_tlbehi = csr_tlbehi.asUInt
  val debug_csr_tlbel0 = csr_tlbel0.asUInt
  val debug_csr_tlbel1 = csr_tlbel1.asUInt
  val debug_csr_asid = csr_asid.asUInt
  val debug_csr_pgdl = csr_pgdl.asUInt
  val debug_csr_pgdh = csr_pgdh.asUInt
  val debug_csr_pgd = csr_pgd.asUInt
  val debug_csr_save0 = csr_save0.asUInt
  val debug_csr_save1 = csr_save1.asUInt
  val debug_csr_save2 = csr_save2.asUInt
  val debug_csr_save3 = csr_save3.asUInt
  val debug_csr_tid = csr_tid.asUInt
  val debug_csr_tcfg = csr_tcfg.asUInt
  val debug_csr_tval = csr_tval.asUInt
  val debug_csr_cntc = csr_cntc.asUInt
  val debug_csr_ticlr = csr_ticlr.asUInt
  val debug_csr_llbctl = csr_llbctl.asUInt
  val debug_csr_tlbrentry = csr_tlbrentry.asUInt
  val debug_csr_dmw0 = csr_dmw0.asUInt
  val debug_csr_dmw1 = csr_dmw1.asUInt
  val debug_timer64 = timer64.asUInt

  dontTouch(debug_csr_crmd)
  dontTouch(debug_csr_prmd)
  dontTouch(debug_csr_ecfg)
  dontTouch(debug_csr_estat)
  dontTouch(debug_csr_era)
  dontTouch(debug_csr_badv)
  dontTouch(debug_csr_eentry)
  dontTouch(debug_csr_tlbidx)
  dontTouch(debug_csr_tlbehi)
  dontTouch(debug_csr_tlbel0)
  dontTouch(debug_csr_tlbel1)
  dontTouch(debug_csr_asid)
  dontTouch(debug_csr_pgdl)
  dontTouch(debug_csr_pgdh)
  dontTouch(debug_csr_pgd)
  dontTouch(debug_csr_save0)
  dontTouch(debug_csr_save1)
  dontTouch(debug_csr_save2)
  dontTouch(debug_csr_save3)
  dontTouch(debug_csr_tid)
  dontTouch(debug_csr_tcfg)
  dontTouch(debug_csr_tval)
  dontTouch(debug_csr_cntc)
  dontTouch(debug_csr_ticlr)
  dontTouch(debug_csr_llbctl)
  dontTouch(debug_csr_tlbrentry)
  dontTouch(debug_csr_dmw0)
  dontTouch(debug_csr_dmw1)
  dontTouch(debug_timer64)

  io.difftest.csr_crmd := csr_crmd.asUInt
  io.difftest.csr_prmd := csr_prmd.asUInt
  io.difftest.csr_ecfg := csr_ecfg.asUInt
  io.difftest.csr_estat := csr_estat.asUInt
  io.difftest.csr_era := csr_era
  io.difftest.csr_badv := csr_badv.asUInt
  io.difftest.csr_eentry := csr_eentry.asUInt
  io.difftest.csr_tlbidx := csr_tlbidx.asUInt
  io.difftest.csr_tlbehi := csr_tlbehi.asUInt
  io.difftest.csr_tlbel0 := csr_tlbel0.asUInt
  io.difftest.csr_tlbel1 := csr_tlbel1.asUInt
  io.difftest.csr_asid := csr_asid.asUInt
  io.difftest.csr_pgdl := csr_pgdl.asUInt
  io.difftest.csr_pgdh := csr_pgdh.asUInt
  io.difftest.csr_pgd := csr_pgd.asUInt
  io.difftest.csr_save0 := csr_save0
  io.difftest.csr_save1 := csr_save1
  io.difftest.csr_save2 := csr_save2
  io.difftest.csr_save3 := csr_save3
  io.difftest.csr_tid := csr_tid
  io.difftest.csr_tcfg := csr_tcfg.asUInt
  io.difftest.csr_tval := csr_tval
  io.difftest.csr_cntc := csr_cntc
  io.difftest.csr_ticlr := csr_ticlr
  io.difftest.csr_llbctl := csr_llbctl.asUInt
  io.difftest.csr_tlbrentry := csr_tlbrentry.asUInt
  io.difftest.csr_dmw0 := csr_dmw0.asUInt
  io.difftest.csr_dmw1 := csr_dmw1.asUInt

  // read
  for(i <- 0 until 2) {
    io.read(i).csr_data := LookupTree(io.read(i).csr_num, Seq(
      CsrName.CRMD  -> csr_crmd.asUInt,
      CsrName.PRMD  -> csr_prmd.asUInt,
      CsrName.ECFG  -> csr_ecfg.asUInt,
      CsrName.ESTAT -> csr_estat.asUInt,
      CsrName.ERA   -> csr_era,
      CsrName.BADV  -> csr_badv,
      CsrName.EENTRY-> csr_eentry.asUInt,
      CsrName.TLBIDX-> csr_tlbidx.asUInt,
      CsrName.TLBEHI-> csr_tlbehi.asUInt,
      CsrName.TLBELO0->csr_tlbel0.asUInt,
      CsrName.TLBELO1->csr_tlbel1.asUInt,
      CsrName.ASID  -> csr_asid.asUInt,
      CsrName.PGDL  -> csr_pgdl.asUInt,
      CsrName.PGDH  -> csr_pgdh.asUInt,
      CsrName.PGD   -> csr_pgd.asUInt,
      CsrName.SAVE0 -> csr_save0,
      CsrName.SAVE1 -> csr_save1,
      CsrName.SAVE2 -> csr_save2,
      CsrName.SAVE3 -> csr_save3,
      CsrName.TID   -> csr_tid,
      CsrName.TCFG  -> csr_tcfg.asUInt,
      CsrName.TVAL  -> csr_tval,
      CsrName.CNTC  -> csr_cntc,
      CsrName.TICLR -> csr_ticlr,
      CsrName.LLBCTL->csr_llbctl.asUInt,
      CsrName.TLBRENTRY->csr_tlbrentry.asUInt,
      CsrName.DMW0  ->csr_dmw0.asUInt,
      CsrName.DMW1  ->csr_dmw1.asUInt
    ))
  }

  // write
  for(i <- 0 until 4) {
    when(io.write(i).valid) {
      switch(io.write(i).bits.csr_num) {
        is(CsrName.CRMD) {
          csr_crmd := io.write(i).bits.csr_data.asTypeOf(new csr_crmd_bundle)
        }
        is(CsrName.EENTRY) {
          csr_eentry := io.write(i).bits.csr_data.asTypeOf(new csr_eentry_bundle)
        }
      }
    }
  }

  // 异常处理
  when(io.exceptionInfo.valid) {
    csr_prmd.pplv := csr_crmd.plv
    csr_prmd.pie := csr_crmd.ie
    csr_crmd.plv := 0.U
    csr_crmd.ie := 0.U

    csr_era := io.exceptionInfo.exceptionPC
    csr_estat.ecode := 11.U                    // FIXME: workaround for syscall
    csr_estat.esubcode := 0.U
  }
  io.exceptionInfo.exceptionNewPC := csr_eentry.asUInt
  io.exceptionInfo.intrNo := Cat(csr_estat.is12, csr_estat.is11, csr_estat.zero10, csr_estat.is9_2)
  io.exceptionInfo.cause := csr_estat.ecode
}