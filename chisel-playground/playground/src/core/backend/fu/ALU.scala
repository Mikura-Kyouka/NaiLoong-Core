package core

import core._ 
import chisel3._
import chisel3.util._ 

import utils._
import core.ALUOpType.add
import core.CSROp.rd
import core.Dest.rj

object ALUOpType {
  def add  = "b1000000".U
  def sll  = "b0000001".U
  def slt  = "b0000010".U
  def sltu = "b0000011".U
  def xor  = "b0000100".U
  def srl  = "b0000101".U
  def or   = "b0000110".U
  def nor  = "b0000000".U
  def and  = "b0000111".U
  def sub  = "b0001000".U
  def sra  = "b0001101".U

  def jirl = "b1011000".U // link 
  def b    = "b1011001".U // no-link 
  def bl   = "b1011010".U // link
  def beq  = "b0010000".U 
  def bne  = "b0010001".U
  def blt  = "b0010100".U
  def bge  = "b0010101".U
  def bltu = "b0010110".U
  def bgeu = "b0010111".U
  def idle = "b0011000".U

  def lu12i = "b1000001".U

  // for RAS
  def call = "b1011100".U
  def ret  = "b1011110".U

  def rriwinz = "b0100000".U

  def isAdd(func: UInt) = func(6)
  def isBru(func: UInt) = func(4)
  def isBranch(func: UInt) = !func(3)
  def isJump(func: UInt) = isBru(func) && !isBranch(func)
  def getBranchType(func: UInt) = func(2, 0)
  // def isBranchInvert(func: UInt) = func(0)
}

object cpucfg {
  def word1 = Cat( 0.U(13.W),    // not used [31:20]
                  31.U( 8.W),    // VALEN    [19:12]
                  31.U( 8.W),    // PALEN    [11: 4]
                   1.U( 1.W),    // PGMMU    [ 3: 3]
                   0.U( 2.W))    // ARCH     [ 2: 1]
  
  def word2 = Cat( 0.U(29.W),    // not used [31: 3]
                   0.U( 1.W),    // FP_DP    [ 2: 2] 
                   0.U( 1.W),    // FP_SP    [ 1: 1]
                   0.U( 1.W))    // FP       [ 0: 0]

  def word16 = Cat( 0.U(25.W),   // not used        [31: 7]
                    0.U( 1.W),   // L2 U_Inclusive  [ 6: 6]
                    0.U( 1.W),   // not used        [ 5: 5]
                    0.U( 2.W),   // L2 U_Present    [ 4: 3]
                    1.U( 1.W),   // L1 D_Present    [ 2: 2]
                    0.U( 1.W),   // not used        [ 1: 1]
                    1.U( 1.W))   // L1 I_Present    [ 0: 0]
  
  def word17 = Cat( 0.U( 1.W),   // not used      [31: 31]
                    4.U( 7.W),   // Linesize-log2 [30: 24]
                   10.U( 8.W),   // Index-log2    [23: 16]
                    0.U(16.W))   // Way-1         [15:  0]

  def word18 = Cat( 0.U( 1.W),   // not used      [31: 31]
                    4.U( 7.W),   // Linesize-log2 [30: 24]
                   11.U( 8.W),   // Index-log2    [23: 16]
                    0.U(16.W))   // Way-1         [15:  0]

  def word19 = Cat( 0.U( 1.W),   // not used      [31: 31]
                    0.U( 7.W),   // Linesize-log2 [30: 24]
                    0.U( 8.W),   // Index-log2    [23: 16]
                    0.U(16.W))   // Way-1         [15:  0]
}

// class FunctionUnitIO extends Bundle {
//   val in = Flipped(Decoupled(new Bundle {
//     val src1 = Output(UInt(32.W))
//     val src2 = Output(UInt(32.W))
//     val func = Output(FuOpType())
//   }))
//   val out = Decoupled(Output(UInt(32.W)))
// } 
// class CtrlFlowIO extends Bundle {
//   val instr = Output(UInt(32.W))
//   val pc = Output(UInt(32.W)) // TODO:VAddrBits
//   val redirect = new RedirectIO
//   val exceptionVec = Output(Vec(16, Bool()))
//   val intrVec = Output(Vec(12, Bool()))
//   val brIdx = Output(UInt(4.W))
//   val crossPageIPFFix = Output(Bool())
//   val runahead_checkpoint_id = Output(UInt(64.W))
//   val isBranch = Output(Bool())
// }
class ALUIO extends FunctionUnitIO {
  val cfIn = Flipped(new CtrlFlowIO)
  val pc = Input(UInt(32.W))
  val redirect_in = Flipped(new RedirectIO)
  val redirect = new RedirectIO
  val offset = Input(UInt(32.W))
}

class ALU extends Module {
  val io = IO(new ALUIO)

  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    io.out.bits
  }

  val isAdderSub = !ALUOpType.isAdd(func) // sub 
  val adderRes = (src1 +& (src2 ^ Fill(32, isAdderSub))) + isAdderSub
  dontTouch(adderRes)
  val xorRes = src1 ^ src2
  val sltu = !adderRes(32)
  val slt = xorRes(31) ^ sltu

  val shsrc1 = src1
  val shamt = src2(4, 0) //shift amount
  val aluRes = MuxLookup(func, adderRes)(
    List(
      ALUOpType.sll  -> ((shsrc1  << shamt)(31, 0)),
      ALUOpType.slt  -> ZeroExt(slt, 32),
      ALUOpType.sltu -> ZeroExt(sltu, 32),
      ALUOpType.xor  -> xorRes,
      ALUOpType.srl  -> (shsrc1  >> shamt),
      ALUOpType.or   -> (src1  |  src2),
      ALUOpType.nor  -> ~(src1  |  src2), 
      ALUOpType.and  -> (src1  &  src2),
      ALUOpType.sra  -> ((shsrc1.asSInt >> shamt).asUInt),
      ALUOpType.lu12i -> src2
    )
  )
  // val aluRes = res

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR, // .orR:所有位都是0,返回false
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt, 
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu,
    ALUOpType.getBranchType(ALUOpType.bne)  -> xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.bge)  -> !slt,
    ALUOpType.getBranchType(ALUOpType.bgeu) -> !sltu
  )

  val isBranch = ALUOpType.isBranch(func)
  val isBru = ALUOpType.isBru(func) //Branch Resolution

  val taken = ALUOpType.isBranch(func) && LookupTree(ALUOpType.getBranchType(func), branchOpTable)  // branch taken

  // if branch type, condition calculation takes over alu, we use another adder
  // else(b, bl, jirl) we use adderRes which calculate dnpc.
  val target = Mux(isBranch, io.pc + io.offset, adderRes)
  dontTouch(target)
  // TODO: Temperarily no branch prediction , we assume predictWrong is always true
  val predictWrong = true.B

  val brValid = (taken || !isBranch) && isBru
  val isJirl = ALUOpType.getBranchType(func) === ALUOpType.getBranchType(ALUOpType.jirl)
  dontTouch(isJirl)
  io.redirect.actuallyTarget := Mux(io.redirect.actuallyTaken, Mux(func === ALUOpType.idle, io.pc, Mux(isBranch || isJirl,
                                                                        target, io.pc + io.offset)), io.pc + 4.U)
  io.redirect.actuallyTaken := brValid || func === ALUOpType.idle
  io.redirect.predictTaken := io.redirect_in.predictTaken
  io.redirect.predictTarget := io.redirect_in.predictTarget
  io.redirect.rtype := DontCare // TODO
  io.redirect.valid := io.in.valid
  
  // actually for bl and jirl to write pc + 4 to rd 
  dontTouch(aluRes)

  val rriImm = io.cfIn.instr(25, 10)
  dontTouch(rriImm)

  val rj_base = rriImm(4, 0)
  val offset = rriImm(9, 5)
  val rd_base = rriImm(14, 10)
  dontTouch(rj_base)
  dontTouch(offset)
  dontTouch(rd_base)

  val a = Wire(UInt(6.W))
  a := rj_base +& offset
  dontTouch(a)

  val b = Wire(UInt(6.W))
  b := rd_base +& offset
  dontTouch(b)

  val aa = Wire(UInt(6.W))
  aa := Mux(a >= 32.U, 32.U, a)
  val bb = Wire(UInt(6.W))
  bb := Mux(b >= 32.U, 32.U, b)
  dontTouch(aa)
  dontTouch(bb)

  val t1 = (1.U << aa) - 1.U
  val t2 = ~((1.U << rj_base) - 1.U)
  val tt = t1 & t2
  dontTouch(t1)
  dontTouch(t2)
  dontTouch(tt)

  val rj_num = PopCount(src1 & tt) % offset
  dontTouch(rj_num)

  val offset_mask = (1.U << offset) - 1.U

  val to_be_ror = ((src2 >> rd_base) & offset_mask)

  dontTouch(to_be_ror)

  val len1 = rj_num
  val len2 = offset - rj_num

  dontTouch(len1)
  dontTouch(len2)

  val right = (to_be_ror & ((1.U << len1) - 1.U))
  val left  = (to_be_ror & (((1.U << len2) - 1.U) << rj_num))

  dontTouch(right)
  dontTouch(left)

  val AAA = right << len2
  val BBB = left >> rj_num
  val CCC = AAA | BBB

  // val ror_num = ((to_be_ror << (offset - rj_num)) | (src2 >> rj_num)) & offset_mask

  val x = (src2 & ~(((1.U << offset) - 1.U) << rd_base)) | (CCC << rd_base)

  // dontTouch(ror_num)
  dontTouch(AAA)
  dontTouch(BBB)
  dontTouch(CCC)
  dontTouch(x)
  dontTouch(offset_mask)

  // ror(src, rd_base, offset, shift):
  //   offset_mask = (1 << shift) - 1
  //   ror_num = ((((src>>rd_base) & offset_mask)<< (offset-shift)) | (src >> shift)) & offset_mask
  //   return (src & ~(offset_mask << rd_base)) | (ror_num << rd_base)

  io.out.bits := Mux(func === ALUOpType.rriwinz, x, Mux(isBru, io.pc + 4.U, aluRes)) // out only has a single 32-bit field
  
  io.in.ready := io.out.ready
  io.out.valid := valid 
}
/*
class inst_info extends Bundle {
  val areg1 = UInt(5.W)
  val areg2 = UInt(5.W)
  val preg1 = UInt(PHYS_REG_BITS.W)
  val preg2 = UInt(PHYS_REG_BITS.W)
  val data1 = UInt(32.W)
  val data2 = UInt(32.W)
  val dest = UInt(PHYS_REG_BITS.W)
  val op = UInt(3.W)

  // use imm
  val imm = UInt(32.W)
  val src2_is_imm = Bool()
}
*/

class FuOut extends Bundle {
  val pc     = Output(UInt(32.W))
  val data   = Output(UInt(32.W))
  val robIdx = Output(UInt(RobConfig.ROB_INDEX_WIDTH.W))
  val preg   = Output(UInt(RegConfig.PHYS_REG_BITS.W))
  val redirect = Output(new RedirectIO)
  val csrNewData = Output(UInt(32.W))
  val exceptionVec = UInt(16.W)
  val tlbInfo = Output(new TlbInstBundle)
  val failsc = Output(Bool()) // 是否发生了失败的sc指令

  // for load/store difftest
  val paddr = Output(UInt(32.W))
  val vaddr = Output(UInt(32.W))
  val wdata = Output(UInt(32.W))
  val fuType = Output(UInt(7.W))
  val optype = Output(UInt(7.W))
  val timer64 = Output(UInt(64.W))
}
class AligendALU extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(Output(new PipelineConnectIO)))
    val out = Decoupled(new FuOut)
    val csrRead = Flipped(new csr_read_bundle)
    val markIntrpt = Input(Bool())
    val cacop = Output(new CACOPIO)
    val excp_en = Input(Bool()) // 是否发生异常
    val ecode = Input(Ecode()) // 异常码
    val flush = Input(Bool())
  })
  
  dontTouch(io.in.bits)
  io.csrRead.csr_num := Mux(io.in.bits.ctrl.csrOp === CSROp.cntid, CsrName.TID, io.in.bits.ctrl.csrNum)
  val dest_is_csr = io.in.bits.ctrl.csrOp === CSROp.rd ||
                    io.in.bits.ctrl.csrOp === CSROp.wr ||
                    io.in.bits.ctrl.csrOp === CSROp.xchg ||
                    io.in.bits.ctrl.csrOp === CSROp.cntid

  val alu = Module(new ALU)
  alu.io := DontCare
  alu.io.cfIn.instr := io.in.bits.instr
  alu.io.redirect_in := io.in.bits.redirect
  alu.io.in.bits.src1 := io.in.bits.src1
  alu.io.in.bits.src2 := Mux(io.in.bits.ctrl.src2Type === 1.U, io.in.bits.imm, io.in.bits.src2)
  alu.io.in.bits.func := io.in.bits.ctrl.fuOpType
  alu.io.offset       := io.in.bits.imm
  alu.io.pc           := io.in.bits.pc
  io.out.bits.pc      := io.in.bits.pc

  val cpucfgWord = MuxLookup(io.in.bits.src1, 0.U)(
    Seq(
      1.U  -> cpucfg.word1,
      2.U  -> cpucfg.word2,
      16.U -> cpucfg.word16,
      17.U -> cpucfg.word17,
      18.U -> cpucfg.word18,
      19.U -> cpucfg.word19
    )
  )

  io.out.bits.data    := Mux(dest_is_csr, io.csrRead.csr_data, Mux(io.in.bits.ctrl.csrOp === CSROp.cntvh, io.csrRead.timer64(63, 32),
                                                               Mux(io.in.bits.ctrl.csrOp === CSROp.cntvl, io.csrRead.timer64(31, 0),
                                                               Mux(io.in.bits.ctrl.csrOp === CSROp.cntvl, io.csrRead.timer64(31, 0),
                                                               Mux(io.in.bits.ctrl.csrOp === CSROp.cpucfg, cpucfgWord,
                                                                alu.io.out.bits)))))
  io.out.bits.robIdx  := io.in.bits.robIdx
  io.out.bits.preg := io.in.bits.preg
  io.out.bits.redirect := alu.io.redirect
  io.out.bits.csrNewData := Mux(io.in.bits.ctrl.csrOp === CSROp.xchg, 
              (io.in.bits.src1 & io.in.bits.src2) | (~io.in.bits.src1 & io.csrRead.csr_data), io.in.bits.src2)
  io.out.bits.tlbInfo.en := io.in.bits.ctrl.tlbOp =/= TlbOp.nop
  io.out.bits.tlbInfo.inst_type := io.in.bits.ctrl.tlbOp
  io.out.bits.tlbInfo.op := io.in.bits.ctrl.tlbInvOp
  io.out.bits.tlbInfo.asid := io.in.bits.src1
  io.out.bits.tlbInfo.va := io.in.bits.src2
  io.out.bits.vaddr := Mux(io.cacop.en, io.cacop.VA, io.in.bits.pc)

  val isCACOP = io.in.bits.ctrl.cType === CACOPType.i && io.in.bits.ctrl.cacopOp =/= CACOPOp.nop && io.in.bits.valid
  val cacopOp2 = isCACOP && io.in.bits.ctrl.cacopOp === CACOPOp.op2

  when(isCACOP) {
    io.cacop.en := true.B && (io.in.valid || RegNext(io.in.valid && cacopOp2))
    io.cacop.op := io.in.bits.ctrl.cacopOp
    io.cacop.VA := alu.io.out.bits
  } .otherwise {
    io.cacop.en := false.B
    io.cacop.op := 0.U
    io.cacop.VA := 0.U
  }

  val op2Reg = RegInit(false.B)
  when(io.in.valid && io.in.bits.valid && io.in.bits.ctrl.cacopOp === CACOPOp.op2) {
    op2Reg := true.B
  }
  when(io.flush || io.out.fire) {
    op2Reg := false.B
  }

  alu.io.in.valid := io.in.valid
  io.in.ready := alu.io.in.ready && (!(io.in.valid && io.in.bits.valid && cacopOp2 && !op2Reg) || io.out.fire) // FIXME 是cacop，并且'没有被接收'，ready不能为高

  io.out.valid := Mux(cacopOp2, 
                      op2Reg, // 延迟一拍再发送
                      alu.io.out.valid && io.in.bits.valid)
  alu.io.out.ready := io.out.ready

  val exceptionVec = Cat(io.excp_en && io.ecode === Ecode.pil,
                        io.excp_en && io.ecode === Ecode.pis,
                        io.excp_en && io.ecode === Ecode.ppi,
                        io.excp_en && io.ecode === Ecode.pme,
                        io.excp_en && io.ecode === Ecode.tlbr,
                        io.in.bits.exceptionVec.asUInt(10, 1),
                        io.markIntrpt)

  io.out.bits.exceptionVec := Cat(io.in.bits.exceptionVec.asUInt(15, 1), io.markIntrpt)

  when(cacopOp2) {
    io.out.bits.exceptionVec := exceptionVec
  }
  // for difftest
  io.out.bits.paddr := DontCare
  io.out.bits.wdata := DontCare
  io.out.bits.fuType := io.in.bits.ctrl.fuType
  io.out.bits.optype := DontCare
  io.out.bits.failsc := false.B // ALU does not handle sc
  io.out.bits.timer64 := io.csrRead.timer64
} 