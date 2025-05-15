package core
import utils._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._


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

class CSRIO extends FunctionUnitIO {

}

class CSR extends Module {
    // val io = IO (new CSRIO)
    // val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)

  val io = IO(new Bundle {
    val read = new csr_read_bundle()
  })
  val csr_crmd = RegInit(0.U.asTypeOf(new csr_crmd_bundle))
  val csr_eentry = RegInit(0.U.asTypeOf(new csr_eentry_bundle))


  io.read.csr_data := LookupTree(io.read.csr_id, Seq(
    CsrName.CRMD -> csr_crmd,
    CsrName.EENTRY -> csr_eentry
  ))
}