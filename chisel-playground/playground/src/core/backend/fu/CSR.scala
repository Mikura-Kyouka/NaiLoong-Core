package core
import utils._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import core.FuType.csr
import java.awt.BufferCapabilities.FlipContents


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

  val CNTVL =69.U(14.W) // 64-bit timer, low part
  val CNTVH =70.U(14.W) // 64-bit timer, high part

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

class CPUCSR extends Module {
    // val io = IO (new CSRIO)
    // val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)

  val io = IO(new Bundle {
    val read = Vec(2, new csr_read_bundle)
    val write = Flipped(Vec(RobConfig.ROB_CMT_NUM, Valid(new csr_write_bundle)))
    val exceptionInfo = new csr_excp_bundle
    val plv = Output(UInt(2.W))
    val llbit = Output(Bool())
    val lladdr = Output(UInt(32.W))
    val markIntrpt = Output(Bool())
    val hardIntrpt = Input(UInt(8.W))
    val difftest = Output(new DiffCSRBundle)

    val to_mmu = Flipped(new CsrToMmuBundle)
    val from_mmu = Flipped(new MmuToCsrBundle)
  })
  val csr_crmd = RegInit(8.U.asTypeOf(new csr_crmd_bundle))  // reset as 0x8
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
  val csr_tval = RegInit("hffffffff".U(32.W))
  val csr_cntc = RegInit(0.U(32.W))
  val csr_ticlr = RegInit(0.U(1.W))
  val csr_llbctl = RegInit(0.U.asTypeOf(new csr_llbctl_bundle))
  val lladdr = RegInit(0.U(32.W))
  val csr_tlbrentry = RegInit(0.U.asTypeOf(new csr_tlbrentry_bundle))
  val csr_dmw0 = RegInit(0.U.asTypeOf(new csr_dmw_bundle))
  val csr_dmw1 = RegInit(0.U.asTypeOf(new csr_dmw_bundle))
  val timer64 = RegInit(0.U(64.W))
  timer64 := timer64 + 1.U
  
  val pgd = WireInit(0.U.asTypeOf(new csr_pgdx_bundle))
  pgd.base := Mux(csr_badv(31) === 0.U, csr_pgdl.base, csr_pgdh.base)
  pgd.zero11_0 := 0.U

  when(csr_tcfg.en === 1.U && csr_tval =/= "hffffffff".U) {
    when(csr_tval === 0.U && csr_tcfg.periodic === 1.U) {
      csr_tval := Cat(csr_tcfg.initval, 0.U(2.W)) // 重新加载定时器值
    }.otherwise {
      csr_tval := csr_tval - 1.U 
    }
  }
  
  csr_estat.is9_2 := io.hardIntrpt

  when(csr_tval === 0.U) {
    csr_estat.is11 := 1.U // 设置定时器中断标志
  }

  io.read(0).timer64 := timer64
  io.read(1).timer64 := timer64

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

  io.plv := csr_crmd.plv

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
  io.difftest.csr_pgd := pgd.asUInt
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
      CsrName.PGD   -> pgd.asUInt,
      CsrName.SAVE0 -> csr_save0,
      CsrName.SAVE1 -> csr_save1,
      CsrName.SAVE2 -> csr_save2,
      CsrName.SAVE3 -> csr_save3,
      CsrName.TID   -> csr_tid,
      CsrName.TCFG  -> csr_tcfg.asUInt,
      CsrName.TVAL  -> csr_tval,
      CsrName.CNTC  -> csr_cntc,
      CsrName.TICLR -> 0.U,
      CsrName.LLBCTL->csr_llbctl.asUInt,
      CsrName.TLBRENTRY->csr_tlbrentry.asUInt,
      CsrName.DMW0  ->csr_dmw0.asUInt,
      CsrName.DMW1  ->csr_dmw1.asUInt,

      CsrName.CNTVL -> timer64(31, 0), // 64-bit timer, low part
      CsrName.CNTVH -> timer64(63, 32) // 64-bit timer, high part
    ))
  }

  // write
  for(i <- 0 until RobConfig.ROB_CMT_NUM) {
    when(io.write(i).valid && csr_crmd.plv === 0.U && !(io.write(i).bits.ll || io.write(i).bits.sc)) { // 只允许PLV0写CSR
      switch(io.write(i).bits.csr_num) {
        is(CsrName.CRMD) {
          csr_crmd := io.write(i).bits.csr_data.asTypeOf(new csr_crmd_bundle)
          // when(io.write(i).bits.csr_data.asTypeOf(new csr_crmd_bundle).pg === 1.U) {  // "推荐"
          //   csr_crmd.datf := 1.U // 设置指令缓存使能
          //   csr_crmd.datm := 1.U // 设置数据缓存使能
          // }
          csr_crmd.zero := 0.U
        }
        is(CsrName.PRMD) {
          csr_prmd := io.write(i).bits.csr_data.asTypeOf(new csr_prmd_bundle)
          csr_prmd.zero := 0.U
        }
        is(CsrName.ECFG) {
          csr_ecfg := io.write(i).bits.csr_data.asTypeOf(new csr_ecfg_bundle)
          csr_ecfg.zero10 := 0.U
          csr_ecfg.zero31_13 := 0.U
        }
        is(CsrName.ESTAT) {
          csr_estat.is1_0 := io.write(i).bits.csr_data(1, 0)
        }
        is(CsrName.EENTRY) {
          csr_eentry := io.write(i).bits.csr_data.asTypeOf(new csr_eentry_bundle)
          csr_eentry.zero := 0.U
        }
        is(CsrName.ERA) {
          csr_era := io.write(i).bits.csr_data
        }
        is(CsrName.BADV) {
          csr_badv := io.write(i).bits.csr_data
        }
        is(CsrName.SAVE0) {
          csr_save0 := io.write(i).bits.csr_data
        }
        is(CsrName.SAVE1) {
          csr_save1 := io.write(i).bits.csr_data
        }
        is(CsrName.SAVE2) {
          csr_save2 := io.write(i).bits.csr_data
        }
        is(CsrName.SAVE3) {
          csr_save3 := io.write(i).bits.csr_data
        }
        is(CsrName.TID) {
          csr_tid := io.write(i).bits.csr_data
        }
        is(CsrName.TCFG) {
          csr_tcfg := io.write(i).bits.csr_data.asTypeOf(new csr_tcfg_bundle)
          when(io.write(i).bits.csr_data(0) === 1.U) {
            csr_tval := Cat(io.write(i).bits.csr_data(31, 2), 0.U(2.W))   // 初始化定时器值
          }
        }
        is(CsrName.TICLR) {
          when(io.write(i).bits.csr_data(0) === 1.U) {
            csr_estat.is11 := 0.U   // 清除定时器中断标志
          }
        }
        is(CsrName.DMW0) {
          csr_dmw0 := io.write(i).bits.csr_data.asTypeOf(new csr_dmw_bundle)
          csr_dmw0.zero2_1 := 0.U
          csr_dmw0.zero24_6 := 0.U
          csr_dmw0.zero28 := 0.U
        }
        is(CsrName.DMW1) {
          csr_dmw1 := io.write(i).bits.csr_data.asTypeOf(new csr_dmw_bundle)
          csr_dmw0.zero2_1 := 0.U
          csr_dmw0.zero24_6 := 0.U
          csr_dmw0.zero28 := 0.U
        }
        is(CsrName.TLBIDX) {
          csr_tlbidx := io.write(i).bits.csr_data.asTypeOf(new csr_tlbidx_bundle)
          csr_tlbidx.zero23_4 := 0.U
          csr_tlbidx.zero30 := 0.U
        }
        is(CsrName.TLBEHI) {
          csr_tlbehi := io.write(i).bits.csr_data.asTypeOf(new csr_tlbehi_bundle)
          csr_tlbehi.zero12_0 := 0.U
        }
        is(CsrName.TLBELO0) {
          csr_tlbel0 := io.write(i).bits.csr_data.asTypeOf(new csr_tlbelo_bundle)
          csr_tlbel0.zero7 := 0.U
          csr_tlbel0.zero31_28 := 0.U
        }
        is(CsrName.TLBELO1) {
          csr_tlbel1 := io.write(i).bits.csr_data.asTypeOf(new csr_tlbelo_bundle)
          csr_tlbel1.zero7 := 0.U
          csr_tlbel1.zero31_28 := 0.U
        }
        is(CsrName.ASID) {
          csr_asid := io.write(i).bits.csr_data.asTypeOf(new csr_asid_bundle)
          csr_asid.zero31_24 := 0.U
          csr_asid.zero15_10 := 0.U
        }
        is(CsrName.PGDL) {
          csr_pgdl := io.write(i).bits.csr_data.asTypeOf(new csr_pgdx_bundle)
          csr_pgdl.zero11_0 := 0.U
        }
        is(CsrName.PGDH) {
          csr_pgdh := io.write(i).bits.csr_data.asTypeOf(new csr_pgdx_bundle)
          csr_pgdh.zero11_0 := 0.U
        }
        is(CsrName.TLBRENTRY) {
          csr_tlbrentry := io.write(i).bits.csr_data.asTypeOf(new csr_tlbrentry_bundle)
          csr_tlbrentry.zero5_0 := 0.U
        }
        is(CsrName.LLBCTL) {
          csr_llbctl.klo := io.write(i).bits.csr_data(2)
          when(io.write(i).bits.csr_data(1) === 1.U) {
            csr_llbctl.rollb := 0.U
          }
        }
      }
    }
  }

  for(i <- 0 until RobConfig.ROB_CMT_NUM) {
    when(io.write(i).valid) {
      when(io.write(i).bits.ll) {
        csr_llbctl.rollb := 1.U
        lladdr := io.write(i).bits.lladdr
      }
      when(io.write(i).bits.sc) {
        csr_llbctl.rollb := 0.U
      }
    }
  }

  io.llbit := csr_llbctl.rollb === 1.U
  io.lladdr := lladdr

  // 中断处理
  val int_vec = csr_ecfg.asUInt(12, 0) & csr_estat.asUInt(12, 0)
  io.markIntrpt := csr_crmd.ie === 1.U && int_vec =/= 0.U

  // 异常处理
  val reversedVec = Reverse(io.exceptionInfo.exceptionVec.asUInt)
  val exceptionIndex = PriorityEncoder(reversedVec)

  // causeTable 的顺序对应 reversedVec 的位编号
  val causeTable = Seq(
    0.U  ->  1.U,  // 原本 bit 15：load 操作页无效
    1.U  ->  2.U,  // 原本 bit 14：store 操作页无效
    2.U  ->  7.U,  // 原本 bit 13：页特权等级错
    3.U  ->  4.U,  // 原本 bit 12：页修改
    4.U  -> 63.U,  // 原本 bit 11：TLB 重填
    5.U  -> 60.U,  // 原本 bit 10：ERET
    6.U  ->  9.U,  // 原本 bit 9：地址非对齐
    7.U  -> 14.U,  // 原本 bit 8：指令特权等级错
    8.U  -> 13.U,  // 原本 bit 7：指令不存在
    9.U  -> 12.U,  // 原本 bit 6：断点
    10.U  -> 11.U,  // 原本 bit 5：系统调用
    11.U  ->  7.U,  // 原本 bit 4：页权限
    12.U  ->  3.U,  // 原本 bit 3：取指页无效
    13.U  -> 63.U,  // 原本 bit 2：TLB 重填
    14.U  ->  8.U,  // 原本 bit 1：取指地址错
    15.U  ->  0.U   // 原本 bit 0：中断
  )

  val defaultCause = 60.U
  val hasException = io.exceptionInfo.exceptionVec.asUInt.orR
  val cause = Mux(hasException, MuxLookup(exceptionIndex, defaultCause)(causeTable), defaultCause)

  io.exceptionInfo.cause := cause

  when(io.exceptionInfo.valid && !io.exceptionInfo.eret) {
    csr_prmd.pplv := csr_crmd.plv
    csr_prmd.pie := csr_crmd.ie
    csr_crmd.plv := 0.U
    csr_crmd.ie := 0.U

    csr_era := io.exceptionInfo.exceptionPC + Mux(io.exceptionInfo.idle, 4.U, 0.U)
    csr_estat.ecode := io.exceptionInfo.cause
    csr_estat.esubcode := 0.U             // TODO: 异常子码
    
    switch(cause) {
      is(3.U) {
        csr_badv := io.exceptionInfo.exceptionPC // 取指页无效
        csr_tlbehi.vppn := io.exceptionInfo.exceptionPC(31, 13)
      }
      is(8.U) {
        csr_badv := io.exceptionInfo.exceptionPC // 取指地址错
        csr_estat.esubcode := 0.U
      }
      is(1.U) {
        csr_badv := io.exceptionInfo.exceptionVAddr // load 操作页无效
        csr_tlbehi.vppn := io.exceptionInfo.exceptionVAddr(31, 13)
      }
      is(2.U) {
        csr_badv := io.exceptionInfo.exceptionVAddr // store 操作页无效
        csr_tlbehi.vppn := io.exceptionInfo.exceptionVAddr(31, 13)
      }
      is(4.U) {
        csr_badv := io.exceptionInfo.exceptionVAddr // 页特权等级错
        csr_tlbehi.vppn := io.exceptionInfo.exceptionVAddr(31, 13)
      }
      is(7.U) {
        csr_badv := io.exceptionInfo.exceptionVAddr // 页修改
        csr_tlbehi.vppn := io.exceptionInfo.exceptionVAddr(31, 13)
      }
      is(9.U) {
        csr_badv := io.exceptionInfo.exceptionVAddr // 地址非对齐
      }
      is(63.U) {                                    
        csr_badv := io.exceptionInfo.exceptionVAddr // TLB 重填
        csr_crmd.da := 1.U
        csr_crmd.pg := 0.U
        csr_tlbehi.vppn := io.exceptionInfo.exceptionVAddr(31, 13)
      }
    }
  }
  io.exceptionInfo.exceptionNewPC := Mux(cause === 63.U, csr_tlbrentry.asUInt, csr_eentry.asUInt)
  io.exceptionInfo.intrNo := Cat(csr_estat.is12, csr_estat.is11, csr_estat.zero10, csr_estat.is9_2)

  when(io.exceptionInfo.eret) {
    csr_crmd.plv := csr_prmd.pplv
    csr_crmd.ie := csr_prmd.pie
    io.exceptionInfo.exceptionNewPC := csr_era
    when(csr_llbctl.klo =/= 1.U) {
      csr_llbctl := 0.U.asTypeOf(new csr_llbctl_bundle) // 清除LLBit
    }
    csr_llbctl.klo := 0.U
    when(csr_estat.ecode === "h3f".U) {
      csr_crmd.da := 0.U
      csr_crmd.pg := 1.U
    }
  }

// to mmu
  io.to_mmu.asid := csr_asid
  io.to_mmu.crmd := csr_crmd
  io.to_mmu.dmw0 := csr_dmw0
  io.to_mmu.dmw1 := csr_dmw1
  io.to_mmu.tlbidx := csr_tlbidx
  io.to_mmu.tlbehi := csr_tlbehi
  io.to_mmu.tlbelo0 := csr_tlbel0
  io.to_mmu.tlbelo1 := csr_tlbel1
  io.to_mmu.estat := csr_estat
// from mmu
  when(io.from_mmu.wen) {
    switch(io.from_mmu.inst_type) {
      is(TlbOp.srch) {
        when(io.from_mmu.tlb_hit) {
          csr_tlbidx.ne := 0.U
          csr_tlbidx.idx := io.from_mmu.tlb_idx
        }.otherwise {
          csr_tlbidx.ne := 1.U
        }
      }
      is(TlbOp.rd) {
        when(io.from_mmu.tlb_entry.e.asBool) {
          csr_tlbidx.ne := ~io.from_mmu.tlb_entry.e
          csr_tlbidx.ps := io.from_mmu.tlb_entry.ps
          csr_tlbehi.vppn := io.from_mmu.tlb_entry.vppn
          csr_asid.asid := io.from_mmu.tlb_entry.asid

          csr_tlbel0.ppn := io.from_mmu.tlb_entry.ppn0
          csr_tlbel0.g := io.from_mmu.tlb_entry.g
          csr_tlbel0.mat := io.from_mmu.tlb_entry.mat0
          csr_tlbel0.plv := io.from_mmu.tlb_entry.plv0
          csr_tlbel0.d := io.from_mmu.tlb_entry.d0
          csr_tlbel0.v := io.from_mmu.tlb_entry.v0

          csr_tlbel1.ppn := io.from_mmu.tlb_entry.ppn1
          csr_tlbel1.g := io.from_mmu.tlb_entry.g
          csr_tlbel1.mat := io.from_mmu.tlb_entry.mat1
          csr_tlbel1.plv := io.from_mmu.tlb_entry.plv1
          csr_tlbel1.d := io.from_mmu.tlb_entry.d1
          csr_tlbel1.v := io.from_mmu.tlb_entry.v1
        }.otherwise {
          csr_tlbidx.ne := 1.U
          csr_tlbidx.ps := 0.U
          csr_asid.asid := 0.U
          csr_tlbehi := 0.U.asTypeOf(new csr_tlbehi_bundle)
          csr_tlbel0 := 0.U.asTypeOf(new csr_tlbelo_bundle)
          csr_tlbel1 := 0.U.asTypeOf(new csr_tlbelo_bundle)
        }
      }
    }
  }
}