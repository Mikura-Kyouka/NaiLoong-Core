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
    val immType :: fuType :: fuOpType :: src1Type :: src2Type :: srcIsRd :: dest :: rfWen :: isLegal :: Nil = 
        ListLookup(instr, Instructions.DecodeDefault, Instructions.DecodeTable)
    io.out.bits := DontCare

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
    io.out.bits.ctrl.rfSrc2 := rfSrc2

    io.out.bits.ctrl.rfWen := rfWen 
    io.out.bits.ctrl.rfDest := rfDest


    io.out.bits.data := DontCare
    val imm = LookupTree(immType, Seq(
        ImmType.si12    -> SignExt(instr(21, 10), 32),  // si12  addi slti sltui l/d[ ] cacop preld
        ImmType.ui12    -> ZeroExt(instr(21, 10), 32),  // ui12 andi ori xori
        ImmType.si16_pc -> SignExt(Cat(instr(25, 10), 0.U(2.W)), 32), // si16_pc jirl beq bne blt[u] bge[u]
        ImmType.si26_pc -> SignExt(Cat(instr(25, 10), instr(9, 0), 0.U(2.W)), 32), // si26_pc b bl
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
    
    //output signals 
    io.out.valid := io.in.valid
    io.in.ready := !io.in.valid // || io.out.fire && !hasIntr
    io.out.bits.cf <> io.in.bits

    //excp_ine 
}

class IDU extends Module {
    val io = IO(new Bundle {
        val in = Vec(4, Flipped(Decoupled(new PipelineConnectIO))) //TODO: Temporarily 4-way
        val out = Vec(4, Decoupled(new DecodeIO))
    })
    val decoder1 = Module(new Decoder)
    val decoder2 = Module(new Decoder)
    val decoder3 = Module(new Decoder)
    val decoder4 = Module(new Decoder)
    io.in(0) <> decoder1.io.in
    io.in(1) <> decoder2.io.in
    io.in(2) <> decoder3.io.in
    io.in(3) <> decoder4.io.in
    
    io.out(0) <> decoder1.io.out
    io.out(1) <> decoder2.io.out
    io.out(2) <> decoder3.io.out
    io.out(3) <> decoder4.io.out
}