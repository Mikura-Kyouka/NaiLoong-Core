package core

import chisel3._ 
import chisel3.util._

import utils._

class Decoder extends Module {
    val io = IO(new Bundle{
        val in = Flipped(Decoupled(new CtrlFlowIO))
        val out = Decoupled(new DecodeIO)
    })

    // val hasInstr = Wire(Bool())
    val instr = io.in.bits.instr
    val immType :: fuType :: fuOpType :: src1Type :: src2Type :: srcIsRd :: dest :: rfWen :: isLegal :: csrOp :: tlbOp :: Nil = 
        ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
    io.out.bits := DontCare

    dontTouch(immType)

    io.out.bits.ctrl.fuType := fuType
    io.out.bits.ctrl.fuOpType := fuOpType

    //  rs2 rs1 
    val (rk, rj, rd) = (instr(14, 10), instr(9, 5), instr(4, 0))
    // rj is equivalent to rs1 in rv ISA, always SLL(R[rj], ui5)
    val rfSrc1 = rj // note: rf is Architectural Register File 
    val rfSrc2 = rk
    val rfDest = LookupTree(dest, Seq(
        Dest.r1 -> 1.U(5.W),
        Dest.rj -> rj,
        Dest.rd -> rd
    ))

    // io.out.bits.ctrl.rfSrc1 := Mux(src1Type === SrcType.pc, 0.U, rfSrc1)
    // io.out.bits.ctrl.rfSrc2 := Mux(src2Type === SrcType.reg, rfSrc2, 0.U)
    /*
    object SrcType {
        def reg = "b0".U
        def pc  = "b1".U
        def imm = "b1".U
        def apply() = UInt(1.W)
    }
    */

    io.out.bits.ctrl.rfSrc1 := rfSrc1
    when (srcIsRd === SrcIsRd.y) (
        io.out.bits.ctrl.rfSrc2 := rd
    ).otherwise (
        io.out.bits.ctrl.rfSrc2 := rfSrc2
    )
    io.out.bits.ctrl.srcIsRd := srcIsRd

    io.out.bits.ctrl.rfWen := rfWen 
    io.out.bits.ctrl.rfDest := rfDest

    io.out.bits.data := DontCare
    val imm = LookupTree(immType, Seq(
        ImmType.si12    -> SignExt(instr(21, 10), 32),  // si12  addi slti sltui l/d[ ] cacop preld
        ImmType.ui12    -> ZeroExt(instr(21, 10), 32),  // ui12 andi ori xori
        ImmType.si16_pc -> SignExt(Cat(instr(25, 10), 0.U(2.W)), 32), // si16_pc jirl beq bne blt[u] bge[u]
        ImmType.si26_pc -> SignExt(Cat(instr(9, 0), instr(25, 10), 0.U(2.W)), 32), // si26_pc b bl
        ImmType.si20    -> Cat(instr(24, 5), 0.U(12.W)), // lu12i_w pcaddu12i
        ImmType.si14_pc -> SignExt(Cat(instr(23, 10), 0.U(2.W)), 32)   //si14_pc ll sc
    ))
    io.out.bits.data.imm := imm
    // // for RAS 
    // when(fuType === FuType.bru) {
    //     def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
    //     when (isLink(rfDest) && fuOpType === ALUOpType.jal){ io.out.bits.ctrl.fuOpType := ALUOpType.call}
    //     when (fuOpType === ALUOpType.jalr) {
    //         when(isLink(rfSrc1)) {io.out.bits.ctrl.fuOpType := ALUOpType.ret }
    //         when(isLink(rfDest)) {io.out.bits.ctrl.fuOpType := ALUOpType.call }
    //     }
    // }
    io.out.bits.ctrl.src1Type := src1Type
    io.out.bits.ctrl.src2Type := src2Type
    // for csr
    io.out.bits.ctrl.csrOp := csrOp
    io.out.bits.ctrl.csrNum := Mux(csrOp === CSROp.cntid, CsrName.TID,
                               Mux(csrOp === CSROp.cntvl, CsrName.CNTVL,
                               Mux(csrOp === CSROp.cntvh, CsrName.CNTVH,
                                                          instr(23, 10))))

    io.out.bits.ctrl.tlbInvOp := Mux(instr(4, 0) === 0.U, TlbInvOp.all, instr(4, 0))

    io.out.bits.ctrl.tlbOp := tlbOp
    
    //output signals 
    io.out.valid := io.in.valid
    io.in.ready := io.out.ready
    io.out.bits.cf <> io.in.bits
    
    // 检查点ID赋值（这样能保证唯一性吗？）
    val seed = RegInit(UInt(16.W), 1234567.U)
    
    when (io.out.bits.ctrl.fuType === FuType.bru) {
        seed := seed + 1.U
    }
    io.out.bits.cf.runahead_checkpoint_id := seed

    //excp_ine 
    /*
    exceptionVec[0]  int
                [1]  adef
                [2]  tlbr    |inst tlb exceptions
                [3]  pif     |
                [4]  ppi     |
                [5]  syscall
                [6]  brk
                [7]  ine
                [8]  ipe
                [9]  ale
                [10] 
                [11] tlbr    |
                [12] pme     |data tlb exceptions
                [13] ppi     |
                [14] pis     |
                [15] pil     |
    */
    when(io.in.bits.pc(1, 0) =/= 0.U) {
        io.out.bits.cf.exceptionVec(1) := true.B // adef
    }.elsewhen(isLegal === IsLegal.n || tlbOp === TlbOp.inv && instr(4, 0) > 6.U) {
        io.out.bits.cf.exceptionVec(7) := true.B // ine
    }.elsewhen(instr(31, 15) === "b00000000001010100".U) {
        io.out.bits.cf.exceptionVec(6) := true.B // brk
    }.elsewhen(csrOp === CSROp.syscall) {
        io.out.bits.cf.exceptionVec(5) := true.B // syscall
    }
}
class IDU extends Module {
    val io = IO(new Bundle {
        // val in = Flipped(Decoupled(Vec(4, new PipelineConnectIO))) //TODO: Temporarily 4-way
        val in = Flipped(Decoupled(Vec(4, new IFU2IDU)))
        val out = Decoupled(Vec(4, new PipelineConnectIO))
    })
    val decoder1 = Module(new Decoder)
    val decoder2 = Module(new Decoder)
    val decoder3 = Module(new Decoder)
    val decoder4 = Module(new Decoder)
    dontTouch(io.out)

//   val instr = Output(UInt(32.W))
//   val pc = Output(UInt(32.W)) // TODO:VAddrBits
//   val redirect = new RedirectIO
//   val exceptionVec = Output(Vec(16, Bool()))
//   val intrVec = Output(Vec(12, Bool()))
//   val brIdx = Output(UInt(4.W))
//   val crossPageIPFFix = Output(Bool())
//   val runahead_checkpoint_id = Output(UInt(64.W))
//   val isBranch = Output(Bool())

    decoder1.io.in.valid := io.in.valid
    decoder2.io.in.valid := io.in.valid
    decoder3.io.in.valid := io.in.valid
    decoder4.io.in.valid := io.in.valid
    decoder1.io.in.bits.tlbr := io.in.bits(0).tlbr
    decoder2.io.in.bits.tlbr := io.in.bits(1).tlbr
    decoder3.io.in.bits.tlbr := io.in.bits(2).tlbr
    decoder4.io.in.bits.tlbr := io.in.bits(3).tlbr
    io.in.ready := decoder1.io.in.ready && decoder2.io.in.ready && decoder3.io.in.ready && decoder4.io.in.ready

    decoder1.io.in.bits.instr := io.in.bits(0).inst
    decoder1.io.in.bits.pc := io.in.bits(0).pc
    decoder1.io.in.bits.redirect := DontCare
    decoder1.io.in.bits.exceptionVec := DontCare
    decoder1.io.in.bits.intrVec := DontCare
    decoder1.io.in.bits.brIdx := DontCare
    decoder1.io.in.bits.crossPageIPFFix := DontCare
    decoder1.io.in.bits.runahead_checkpoint_id := DontCare
    decoder1.io.in.bits.isBranch := DontCare
    decoder2.io.in.bits.instr := io.in.bits(1).inst
    decoder2.io.in.bits.pc := io.in.bits(1).pc
    decoder2.io.in.bits.redirect := DontCare
    decoder2.io.in.bits.exceptionVec := DontCare
    decoder2.io.in.bits.intrVec := DontCare
    decoder2.io.in.bits.brIdx := DontCare
    decoder2.io.in.bits.crossPageIPFFix := DontCare
    decoder2.io.in.bits.runahead_checkpoint_id := DontCare
    decoder2.io.in.bits.isBranch := DontCare
    decoder3.io.in.bits.instr := io.in.bits(2).inst
    decoder3.io.in.bits.pc := io.in.bits(2).pc
    decoder3.io.in.bits.redirect := DontCare
    decoder3.io.in.bits.exceptionVec := DontCare
    decoder3.io.in.bits.intrVec := DontCare
    decoder3.io.in.bits.brIdx := DontCare
    decoder3.io.in.bits.crossPageIPFFix := DontCare
    decoder3.io.in.bits.runahead_checkpoint_id := DontCare
    decoder3.io.in.bits.isBranch := DontCare
    decoder4.io.in.bits.instr := io.in.bits(3).inst
    decoder4.io.in.bits.pc := io.in.bits(3).pc
    decoder4.io.in.bits.redirect := DontCare
    decoder4.io.in.bits.exceptionVec := DontCare
    decoder4.io.in.bits.intrVec := DontCare
    decoder4.io.in.bits.brIdx := DontCare
    decoder4.io.in.bits.crossPageIPFFix := DontCare
    decoder4.io.in.bits.runahead_checkpoint_id := DontCare
    decoder4.io.in.bits.isBranch := DontCare
    
    io.out.valid := decoder1.io.out.valid && decoder2.io.out.valid && decoder3.io.out.valid && decoder4.io.out.valid
    decoder1.io.out.ready := io.out.ready
    decoder2.io.out.ready := io.out.ready
    decoder3.io.out.ready := io.out.ready
    decoder4.io.out.ready := io.out.ready
    
    io.out.bits(0).instr := decoder1.io.out.bits.cf.instr
    io.out.bits(0).pc := decoder1.io.out.bits.cf.pc
    io.out.bits(0).valid := io.in.bits(0).Valid
    io.out.bits(0).redirect := decoder1.io.out.bits.cf.redirect
    io.out.bits(0).exceptionVec := decoder1.io.out.bits.cf.exceptionVec
    io.out.bits(0).intrVec := decoder1.io.out.bits.cf.intrVec
    io.out.bits(0).brIdx := decoder1.io.out.bits.cf.brIdx
    io.out.bits(0).crossPageIPFFix := decoder1.io.out.bits.cf.crossPageIPFFix
    io.out.bits(0).checkpoint.id := decoder1.io.out.bits.cf.runahead_checkpoint_id
    io.out.bits(0).checkpoint.needSave := decoder1.io.out.bits.ctrl.fuType === FuType.bru
    io.out.bits(0).isBranch := decoder1.io.out.bits.cf.isBranch
    io.out.bits(0).ctrl <> decoder1.io.out.bits.ctrl
    io.out.bits(0).src1 := decoder1.io.out.bits.data.src1
    io.out.bits(0).src2 := decoder1.io.out.bits.data.src2
    io.out.bits(0).imm := decoder1.io.out.bits.data.imm
    io.out.bits(1).instr := decoder2.io.out.bits.cf.instr
    io.out.bits(1).pc := decoder2.io.out.bits.cf.pc
    io.out.bits(1).valid := io.in.bits(1).Valid
    io.out.bits(1).redirect := decoder2.io.out.bits.cf.redirect
    io.out.bits(1).exceptionVec := decoder2.io.out.bits.cf.exceptionVec
    io.out.bits(1).intrVec := decoder2.io.out.bits.cf.intrVec
    io.out.bits(1).brIdx := decoder2.io.out.bits.cf.brIdx
    io.out.bits(1).crossPageIPFFix := decoder2.io.out.bits.cf.crossPageIPFFix
    io.out.bits(1).checkpoint.id := decoder2.io.out.bits.cf.runahead_checkpoint_id
    io.out.bits(1).checkpoint.needSave := decoder2.io.out.bits.ctrl.fuType === FuType.bru
    io.out.bits(1).isBranch := decoder2.io.out.bits.cf.isBranch
    io.out.bits(1).ctrl <> decoder2.io.out.bits.ctrl
    io.out.bits(1).src1 := decoder2.io.out.bits.data.src1
    io.out.bits(1).src2 := decoder2.io.out.bits.data.src2
    io.out.bits(1).imm := decoder2.io.out.bits.data.imm
    io.out.bits(2).instr := decoder3.io.out.bits.cf.instr
    io.out.bits(2).pc := decoder3.io.out.bits.cf.pc
    io.out.bits(2).valid := io.in.bits(2).Valid
    io.out.bits(2).redirect := decoder3.io.out.bits.cf.redirect
    io.out.bits(2).exceptionVec := decoder3.io.out.bits.cf.exceptionVec
    io.out.bits(2).intrVec := decoder3.io.out.bits.cf.intrVec
    io.out.bits(2).brIdx := decoder3.io.out.bits.cf.brIdx
    io.out.bits(2).crossPageIPFFix := decoder3.io.out.bits.cf.crossPageIPFFix
    io.out.bits(2).checkpoint.id := decoder3.io.out.bits.cf.runahead_checkpoint_id
    io.out.bits(2).checkpoint.needSave := decoder3.io.out.bits.ctrl.fuType === FuType.bru
    io.out.bits(2).isBranch := decoder3.io.out.bits.cf.isBranch
    io.out.bits(2).ctrl <> decoder3.io.out.bits.ctrl
    io.out.bits(2).src1 := decoder3.io.out.bits.data.src1
    io.out.bits(2).src2 := decoder3.io.out.bits.data.src2
    io.out.bits(2).imm := decoder3.io.out.bits.data.imm
    io.out.bits(3).instr := decoder4.io.out.bits.cf.instr
    io.out.bits(3).pc := decoder4.io.out.bits.cf.pc
    io.out.bits(3).valid := io.in.bits(3).Valid
    io.out.bits(3).redirect := decoder4.io.out.bits.cf.redirect
    io.out.bits(3).exceptionVec := decoder4.io.out.bits.cf.exceptionVec
    io.out.bits(3).intrVec := decoder4.io.out.bits.cf.intrVec
    io.out.bits(3).brIdx := decoder4.io.out.bits.cf.brIdx
    io.out.bits(3).crossPageIPFFix := decoder4.io.out.bits.cf.crossPageIPFFix
    io.out.bits(3).checkpoint.id := decoder4.io.out.bits.cf.runahead_checkpoint_id
    io.out.bits(3).checkpoint.needSave := decoder4.io.out.bits.ctrl.fuType === FuType.bru
    io.out.bits(3).isBranch := decoder4.io.out.bits.cf.isBranch
    io.out.bits(3).ctrl <> decoder4.io.out.bits.ctrl
    io.out.bits(3).src1 := decoder4.io.out.bits.data.src1
    io.out.bits(3).src2 := decoder4.io.out.bits.data.src2
    io.out.bits(3).imm := decoder4.io.out.bits.data.imm

    for (i <- 0 until 4) {
        io.out.bits(i).redirect := io.in.bits(i).brPredict
        io.out.bits(i).prj := DontCare
        io.out.bits(i).jIsArf := DontCare
        io.out.bits(i).dataj := DontCare
        io.out.bits(i).prk := DontCare
        io.out.bits(i).kIsArf := DontCare
        io.out.bits(i).datak := DontCare
        io.out.bits(i).preg := DontCare
        io.out.bits(i).old_preg := DontCare
        io.out.bits(i).robIdx := DontCare
        io.out.bits(i).csrNewData := DontCare
    }
    
}

// class PipelineConnectIO extends Bundle {
//   // common
//   val instr = Output(UInt(32.W))
//   val pc = Output(UInt(32.W))
//   val valid = Output(Bool())
//   // IF -> ID
//   val redirect = new RedirectIO
//   val exceptionVec = Output(Vec(16, Bool()))
//   val intrVec = Output(Vec(12, Bool()))
//   val brIdx = Output(UInt(4.W))
//   val crossPageIPFFix = Output(Bool())
//   val runahead_checkpoint_id = Output(UInt(64.W))
//   val isBranch = Output(Bool())
//   // ID -> Renaming
//   val src1 = Output(UInt(32.W))
//   val src2 = Output(UInt(32.W))
//   val imm  = Output(UInt(32.W))
//   val ctrl = new CtrlSignalIO
// }