package core

import core._
import chisel3._ 
import chisel3.util._

object Inst {
    def add_w           = BitPat("b00000000000100000_?????_?????_?????")     // n2_add_w 
    def sub_w           = BitPat("b00000000000100010_?????_?????_?????")     // n4_sub_w 
    def slt             = BitPat("b00000000000100100_?????_?????_?????")     // n5_slt 
    def sltu            = BitPat("b00000000000100101_?????_?????_?????")     // n6_sltu 
    def nor             = BitPat("b00000000000101000_?????_?????_?????")     // n10_nor 
    def and             = BitPat("b00000000000101001_?????_?????_?????")     // n7_and 
    def or              = BitPat("b00000000000101010_?????_?????_?????")     // n8_or 
    def xor             = BitPat("b00000000000101011_?????_?????_?????")     // n9_xor 
    def mul_w           = BitPat("b00000000000111000_?????_?????_?????")     // n32_mul_w 
    def mulh_w          = BitPat("b00000000000111001_?????_?????_?????")     // n33_mulh_w 
    def mulh_wu         = BitPat("b00000000000111010_?????_?????_?????")     // n34_mulh_wu 
    def div_w           = BitPat("b00000000001000000_?????_?????_?????")     // n30_div_w 
    def mod_w           = BitPat("b00000000001000001_?????_?????_?????")     // n35_mod_w 
    def div_wu          = BitPat("b00000000001000010_?????_?????_?????")     // n31_div_wu 
    def mod_wu          = BitPat("b00000000001000011_?????_?????_?????")     // n36_mod_wu 
    
    def sll_w           = BitPat("b00000000000101110_?????_?????_?????")     // n27_sll_w 
    def srl_w           = BitPat("b00000000000101111_?????_?????_?????")     // n29_srl_w 
    def sra_w           = BitPat("b00000000000110000_?????_?????_?????")     // n28_sra_w 
    def slli_w          = BitPat("b00000000010000001_?????_?????_?????")     // n11_slli_w 
    def srli_w          = BitPat("b00000000010001001_?????_?????_?????")     // n12_srli_w 
    def srai_w          = BitPat("b00000000010010001_?????_?????_?????")     // n13_srai_w 

    def ld_b            = BitPat("b0010100000_????????????_?????_?????")     // n41_ld_b 
    def ld_h            = BitPat("b0010100001_????????????_?????_?????")     // n42_ld_h
    def ld_w            = BitPat("b0010100010_????????????_?????_?????")     // n14_ld_w  
    def st_b            = BitPat("b0010100100_????????????_?????_?????")     // n45_st_b 
    def st_h            = BitPat("b0010100101_????????????_?????_?????")     // n46_st_h 
    def st_w            = BitPat("b0010100110_????????????_?????_?????")     // n15_st_w 
    def ld_bu           = BitPat("b0010101000_????????????_?????_?????")     // n43_ld_bu 
    def ld_hu           = BitPat("b0010101001_????????????_?????_?????")     // n44_ld_hu  
    def preld           = BitPat("b0010101011_????????????_?????_?????")     // 
    
    /* 原子访存指令 */
    def ll_w            = BitPat("b00100000_??????????????_?????_?????")
    def sc_w            = BitPat("b00100001_??????????????_?????_?????")

    def jirl            = BitPat("b010011_????????????????_?????_?????")     // n19_jirl 
    def b               = BitPat("b010100_????????????????_??????????")     // n20_b 
    def bl              = BitPat("b010101_????????????????_??????????")     // n18_bl 
    def beq             = BitPat("b010110_????????????????_?????_?????")     // n16_beq 
    def bne             = BitPat("b010111_????????????????_?????_?????")     // n17_bne 
    def blt             = BitPat("b011000_????????????????_?????_?????")     // n37_blt 
    def bge             = BitPat("b011001_????????????????_?????_?????")     // n38_bge 
    def bltu            = BitPat("b011010_????????????????_?????_?????")     // n39_bltu 
    def bgeu            = BitPat("b011011_????????????????_?????_?????")     // n40_bgeu 

    def lu12i_w         = BitPat("b0001010_????????????????????_?????")     // n1_lu12i_w 
    def pcaddu12i       = BitPat("b0001110_????????????????????_?????")     // n21_pcaddu12i 
    
    def slti            = BitPat("b0000001000_????????????_?????_?????")     // n22_slti 
    def sltui           = BitPat("b0000001001_????????????_?????_?????")     // n23_sltui 
    def addi_w          = BitPat("b0000001010_????????????_?????_?????")     // n3_addi_w 
    def andi            = BitPat("b0000001101_????????????_?????_?????")     // n24_andi 
    def ori             = BitPat("b0000001110_????????????_?????_?????")     // n25_ori 
    def xori            = BitPat("b0000001111_????????????_?????_?????")     // n26_xori 
    
    def syscall         = BitPat("b00000000001010110_?????_?????_?????")     // n47_syscall_ex 
    def break           = BitPat("b00000000001010100_?????_?????_?????")     // n48_brk_ex 
    def csrrd           = BitPat("b00000100_??????????????_00000_?????")
    def csrwr           = BitPat("b00000100_??????????????_00001_?????")
    def csrxchg         = BitPat("b00000100_??????????????_?????_?????")     // n49_ti_ex 
    
    /* 栅障指令 */
    def dbar            = BitPat("b00111000011100100_???????????????")
    def ibar            = BitPat("b00111000011100101_???????????????")

    def rdcntid_w       = BitPat("b0000000000000000011000_?????_00000") // RDCNTID Counter ID 号信息写入通用寄存器 rj中
    def rdcntvl_w       = BitPat("b0000000000000000011000_00000_?????") // 低位
    def rdcntvh_w       = BitPat("b0000000000000000011001_00000_?????") // 高位     // n58_rdcnt
    
    /* CPU 配置获取指令 */
    def cpucfg          = BitPat("b0000000000000000011011_?????_?????")

    /* Cache维护指令 */
    def icacop_op0      = BitPat("b0000011000_????????????_?????_00000")
    def icacop_op1      = BitPat("b0000011000_????????????_?????_01000")
    def icacop_op2      = BitPat("b0000011000_????????????_?????_10000")
    def dcacop_op0      = BitPat("b0000011000_????????????_?????_00001")
    def dcacop_op1      = BitPat("b0000011000_????????????_?????_01001")
    def dcacop_op2      = BitPat("b0000011000_????????????_?????_10001")
    def cacop_nop       = BitPat("b0000011000_????????????_?????_???10")

    /* TLB维护指令 */
    def tlbsrch         = BitPat("b00000110010010000010100000000000")
    def tlbrd           = BitPat("b00000110010010000010110000000000")
    def tlbwr           = BitPat("b00000110010010000011000000000000")
    def tlbfill         = BitPat("b00000110010010000011010000000000")
    def invtlb          = BitPat("b00000110010010011_?????_?????_?????")
    
    def ertn            = BitPat("b00000110010010000011100000000000")
    def idle            = BitPat("b00000110010010001_???????????????")
    
    val table = Array(
        add_w           -> List(ImmType.nop     , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop),  
        sub_w           -> List(ImmType.nop     , FuType.alu, ALUOpType.sub,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        slt             -> List(ImmType.nop     , FuType.alu, ALUOpType.slt,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        sltu            -> List(ImmType.nop     , FuType.alu, ALUOpType.sltu,  SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        nor             -> List(ImmType.nop     , FuType.alu, ALUOpType.nor,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        and             -> List(ImmType.nop     , FuType.alu, ALUOpType.and,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        or              -> List(ImmType.nop     , FuType.alu, ALUOpType.or,    SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        xor             -> List(ImmType.nop     , FuType.alu, ALUOpType.xor,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        mul_w           -> List(ImmType.nop     , FuType.mdu, MDUOpType.mul,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        mulh_w          -> List(ImmType.nop     , FuType.mdu, MDUOpType.mulh,  SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        mulh_wu         -> List(ImmType.nop     , FuType.mdu, MDUOpType.mulhu, SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        div_w           -> List(ImmType.nop     , FuType.mdu, MDUOpType.div,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        mod_w           -> List(ImmType.nop     , FuType.mdu, MDUOpType.mod,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        div_wu          -> List(ImmType.nop     , FuType.mdu, MDUOpType.divu,  SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        mod_wu          -> List(ImmType.nop     , FuType.mdu, MDUOpType.modu,  SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        sll_w           -> List(ImmType.nop     , FuType.alu, ALUOpType.sll,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        srl_w           -> List(ImmType.nop     , FuType.alu, ALUOpType.srl,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        sra_w           -> List(ImmType.nop     , FuType.alu, ALUOpType.sra,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        slli_w          -> List(ImmType.si12    , FuType.alu, ALUOpType.sll,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        srli_w          -> List(ImmType.si12    , FuType.alu, ALUOpType.srl,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        srai_w          -> List(ImmType.si12    , FuType.alu, ALUOpType.sra,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ld_b            -> List(ImmType.si12    , FuType.lsu, LSUOpType.lb,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ld_h            -> List(ImmType.si12    , FuType.lsu, LSUOpType.lh,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ld_w            -> List(ImmType.si12    , FuType.lsu, LSUOpType.lw,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        st_b            -> List(ImmType.si12    , FuType.lsu, LSUOpType.sb,    SrcType.reg, SrcType.imm, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        st_h            -> List(ImmType.si12    , FuType.lsu, LSUOpType.sh,    SrcType.reg, SrcType.imm, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        st_w            -> List(ImmType.si12    , FuType.lsu, LSUOpType.sw,    SrcType.reg, SrcType.imm, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ld_bu           -> List(ImmType.si12    , FuType.lsu, LSUOpType.lbu,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ld_hu           -> List(ImmType.si12    , FuType.lsu, LSUOpType.lhu,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        preld           -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ll_w            -> List(ImmType.si14_pc , FuType.lsu, LSUOpType.ll,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.ll,      TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        sc_w            -> List(ImmType.si14_pc , FuType.lsu, LSUOpType.sc,    SrcType.reg, SrcType.imm, SrcIsRd.y, Dest.rd, RfWen.y, IsLegal.y, CSROp.sc,      TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        jirl            -> List(ImmType.si16_pc , FuType.bru, ALUOpType.jirl,  SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        b               -> List(ImmType.si26_pc , FuType.bru, ALUOpType.b,     SrcType.pc , SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        bl              -> List(ImmType.si26_pc , FuType.bru, ALUOpType.bl,    SrcType.pc , SrcType.imm, SrcIsRd.n, Dest.r1, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        beq             -> List(ImmType.si16_pc , FuType.bru, ALUOpType.beq,   SrcType.reg, SrcType.reg, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        bne             -> List(ImmType.si16_pc , FuType.bru, ALUOpType.bne,   SrcType.reg, SrcType.reg, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        blt             -> List(ImmType.si16_pc , FuType.bru, ALUOpType.blt,   SrcType.reg, SrcType.reg, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        bge             -> List(ImmType.si16_pc , FuType.bru, ALUOpType.bge,   SrcType.reg, SrcType.reg, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        bltu            -> List(ImmType.si16_pc , FuType.bru, ALUOpType.bltu,  SrcType.reg, SrcType.reg, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        bgeu            -> List(ImmType.si16_pc , FuType.bru, ALUOpType.bgeu,  SrcType.reg, SrcType.reg, SrcIsRd.y, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        lu12i_w         -> List(ImmType.si20    , FuType.alu, ALUOpType.lu12i, SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        pcaddu12i       -> List(ImmType.si20    , FuType.alu, ALUOpType.add,   SrcType.pc , SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        slti            -> List(ImmType.si12    , FuType.alu, ALUOpType.slt,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        sltui           -> List(ImmType.si12    , FuType.alu, ALUOpType.sltu,  SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        addi_w          -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        andi            -> List(ImmType.ui12    , FuType.alu, ALUOpType.and,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ori             -> List(ImmType.ui12    , FuType.alu, ALUOpType.or,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        xori            -> List(ImmType.ui12    , FuType.alu, ALUOpType.xor,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        syscall         -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.syscall, TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        break           -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        csrrd           -> List(ImmType.nop     , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.rd,      TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        csrwr           -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.y, Dest.rd, RfWen.y, IsLegal.y, CSROp.wr,      TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        csrxchg         -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.y, Dest.rd, RfWen.y, IsLegal.y, CSROp.xchg,    TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        dbar            -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        ibar            -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        rdcntid_w       -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rj, RfWen.y, IsLegal.y, CSROp.cntid,   TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        rdcntvl_w       -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.cntvl,   TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        rdcntvh_w       -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.cntvh,   TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        cpucfg          -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.y, IsLegal.y, CSROp.cpucfg,  TlbOp.nop,  CACOPType.i, CACOPOp.nop),
        icacop_op0      -> List(ImmType.si12    , FuType.bru, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.op0), 
        icacop_op1      -> List(ImmType.si12    , FuType.bru, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.op1), 
        icacop_op2      -> List(ImmType.si12    , FuType.bru, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.op2), 
        dcacop_op0      -> List(ImmType.si12    , FuType.lsu, LSUOpType.lw,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.d, CACOPOp.op0), 
        dcacop_op1      -> List(ImmType.si12    , FuType.lsu, LSUOpType.lw,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.d, CACOPOp.op1), 
        dcacop_op2      -> List(ImmType.si12    , FuType.lsu, LSUOpType.lw,    SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.d, CACOPOp.op2), 
        cacop_nop       -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop),
        tlbsrch         -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.srch, CACOPType.i, CACOPOp.nop), 
        tlbrd           -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.rd,   CACOPType.i, CACOPOp.nop), 
        tlbwr           -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.wr,   CACOPType.i, CACOPOp.nop), 
        tlbfill         -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.fill, CACOPType.i, CACOPOp.nop), 
        invtlb          -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.reg, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.inv,  CACOPType.i, CACOPOp.nop), 
        ertn            -> List(ImmType.si12    , FuType.alu, ALUOpType.add,   SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.ertn,    TlbOp.nop,  CACOPType.i, CACOPOp.nop), 
        idle            -> List(ImmType.si12    , FuType.bru, ALUOpType.idle,  SrcType.reg, SrcType.imm, SrcIsRd.n, Dest.rd, RfWen.n, IsLegal.y, CSROp.nop,     TlbOp.nop,  CACOPType.i, CACOPOp.nop),
    )
}