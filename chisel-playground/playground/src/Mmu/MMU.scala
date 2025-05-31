package core
import chisel3._
import MmuConfig._

object MMUPipelineConnect {
  import chisel3.util._
  def apply[T <: Data](
    left: DecoupledIO[T],
    right: DecoupledIO[T],
    rightOutFire: Bool,
    isFlush: Bool
  ) = {
    val s_idle :: s_in  :: Nil = Enum(2)
    val state = RegInit(s_idle)

    val reg = RegInit(0.U.asTypeOf(left.bits))
    when(left.valid && right.ready) {
      reg := left.bits
    }
    
    state := MuxLookup(state, s_idle)(Seq(
      s_idle -> Mux(left.valid && right.ready, s_in, s_idle),
      s_in -> Mux(rightOutFire, Mux(left.valid && right.ready, s_in, s_idle), s_in)
    ))

    // orginal code
    /*
    when(state === s_in) {
      right.bits := reg
    }.otherwise{
      right.bits := reg
      right.bits match {
        case b: IDU2EXU => b.regW := false.B
        case b: EXU2LSU => b.regW := false.B
        case _ => // 如果不是 IDU2EXU 或 EXU2LSU，不做任何操作
      }
      right.bits match {
        case b: EXU2LSU => b.needMem := false.B
        case _ => 
      }
    }
    */
    // new code
    right.bits := reg

    val valid = RegInit(false.B)
    when(rightOutFire) { valid := false.B } // already excepted, right.valid := false
    when(left.valid && right.ready) { valid := true.B } // in.fire
    when(isFlush) { valid := false.B }

    left.ready := right.ready
    // right.bits := RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //&& !isFlush
  }
}

/* 
  val s1 = Module(new MMUStage1)
  val s2 = Module(new MMUStage2)
  val tlb = RegInit(***)

  s1.tlb <> tlb
  s2.tlb <> tlb

  MMUPipelineConnect(s1.io.in, s2.io.in, s2.io.out.fire, io.isFlush)
 */

class MMUStage1 extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in0 = Input(new AddrTrans)
    val in1 = Input(new AddrTrans)

    val tlb_interface0 = new Stage1Interface
    val tlb_interface1 = new Stage1Interface

    val out0 = Decoupled(new AddrTrans)
    val out1 = Decoupled(new AddrTrans)
  })

  io.out0.bits := io.in0
  io.out1.bits := io.in1

  io.tlb_interface0.vppn := io.in0.vaddr(ADDR_WIDTH - 1, PAGE_WIDTH + 1)
  io.tlb_interface1.vppn := io.in1.vaddr(ADDR_WIDTH - 1, PAGE_WIDTH + 1)

  io.out0.bits.hit_vec := io.tlb_interface0.hit_vec
  io.out1.bits.hit_vec := io.tlb_interface1.hit_vec

  io.out0.valid := io.in0.trans_en
  io.out1.valid := io.in1.trans_en
}

class MMUStage2 extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in0 = Flipped(Decoupled(new AddrTrans))
    val in1 = Flipped(Decoupled(new AddrTrans))

    val tlb_interface0 = new Stage2Interface
    val tlb_interface1 = new Stage2Interface

    val out0 = Decoupled(new AddrTrans)
    val out1 = Decoupled(new AddrTrans)
  })

  io.out0.bits := io.in0.bits
  io.out1.bits := io.in1.bits

  io.tlb_interface0.vppn := io.in0.bits.vaddr(ADDR_WIDTH - 1, PAGE_WIDTH + 1)
  io.tlb_interface1.vppn := io.in1.bits.vaddr(ADDR_WIDTH - 1, PAGE_WIDTH + 1)
  io.tlb_interface0.va_bit12 := io.in0.bits.vaddr(12)
  io.tlb_interface1.va_bit12 := io.in1.bits.vaddr(12)
  io.tlb_interface0.hit_vec := io.in0.bits.hit_vec
  io.tlb_interface1.hit_vec := io.in1.bits.hit_vec

  io.out0.bits.found := io.tlb_interface0.found
  io.out1.bits.found := io.tlb_interface1.found
  io.out0.bits.index := io.tlb_interface0.index
  io.out1.bits.index := io.tlb_interface1.index
  io.out0.bits.ppn := io.tlb_interface0.ppn
  io.out1.bits.ppn := io.tlb_interface1.ppn
  io.out0.bits.ps := io.tlb_interface0.ps
  io.out1.bits.ps := io.tlb_interface1.ps
  io.out0.bits.plv := io.tlb_interface0.plv
  io.out1.bits.plv := io.tlb_interface1.plv
  io.out0.bits.mat := io.tlb_interface0.mat
  io.out1.bits.mat := io.tlb_interface1.mat
  io.out0.bits.d := io.tlb_interface0.d
  io.out1.bits.d := io.tlb_interface1.d

  io.out0.valid := io.in0.valid
  io.out1.valid := io.in1.valid
  io.in0.ready := io.out0.ready
  io.in1.ready := io.out1.ready
}

class TLB extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val s1_interface0 = Flipped(new Stage1Interface)
    val s1_interface1 = Flipped(new Stage1Interface)

    val csr = new CsrToMmuBundle

    val s2_interface0 = Flipped(new Stage2Interface)
    val s2_interface1 = Flipped(new Stage2Interface)
  })

  val tlb = Reg(Vec(TLB_NUM, new TlbBundle))

// 处理 Stage 1 : 判断是否命中 TLB
  val hit0 = Wire(Vec(TLB_NUM, Bool()))
  val hit1 = Wire(Vec(TLB_NUM, Bool()))

  // 设计实战 P51
  for (i <- 0 until TLB_NUM) {
    hit0(i) := (tlb(i).e.asBool) && 
               ((tlb(i).g.asBool) || (tlb(i).asid === io.csr.asid.asid)) &&
               (tlb(i).vppn === io.s1_interface0.vppn)
  }
  for (i <- 0 until TLB_NUM) {
    hit1(i) := (tlb(i).e.asBool) && 
               ((tlb(i).g.asBool) || (tlb(i).asid === io.csr.asid.asid)) &&
               (tlb(i).vppn === io.s1_interface1.vppn)
  }

  io.s1_interface0.hit_vec := hit0
  io.s1_interface1.hit_vec := hit1

// 处理 Stage 2
  // 地址翻译
  val hit0_index = OHToUInt(io.s2_interface0.hit_vec.asUInt)
  val hit1_index = OHToUInt(io.s2_interface1.hit_vec.asUInt)

  io.s2_interface0.found := io.s2_interface0.hit_vec.asUInt.orR
  io.s2_interface0.index := hit0_index
  io.s2_interface0.ppn := Mux(io.s2_interface0.va_bit12.asBool, tlb(hit0_index).ppn1, tlb(hit0_index).ppn0)
  io.s2_interface0.ps := PAGE_WIDTH.U
  io.s2_interface0.plv := Mux(io.s2_interface0.va_bit12.asBool, tlb(hit0_index).plv1, tlb(hit0_index).plv0)
  io.s2_interface0.mat := Mux(io.s2_interface0.va_bit12.asBool, tlb(hit0_index).mat1, tlb(hit0_index).mat0)
  io.s2_interface0.d := Mux(io.s2_interface0.va_bit12.asBool, tlb(hit0_index).d1, tlb(hit0_index).d0)
  io.s2_interface0.v := Mux(io.s2_interface0.va_bit12.asBool, tlb(hit0_index).v1, tlb(hit0_index).v0)

  io.s2_interface1.found := io.s2_interface1.hit_vec.asUInt.orR
  io.s2_interface1.index := hit1_index
  io.s2_interface1.ppn := Mux(io.s2_interface1.va_bit12.asBool, tlb(hit1_index).ppn1, tlb(hit1_index).ppn0)
  io.s2_interface1.ps := PAGE_WIDTH.U
  io.s2_interface1.plv := Mux(io.s2_interface1.va_bit12.asBool, tlb(hit1_index).plv1, tlb(hit1_index).plv0)
  io.s2_interface1.mat := Mux(io.s2_interface1.va_bit12.asBool, tlb(hit1_index).mat1, tlb(hit1_index).mat0)
  io.s2_interface1.d := Mux(io.s2_interface1.va_bit12.asBool, tlb(hit1_index).d1, tlb(hit1_index).d0)
  io.s2_interface1.v := Mux(io.s2_interface1.va_bit12.asBool, tlb(hit1_index).v1, tlb(hit1_index).v0)
}

class MMU extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in0 = Input(new AddrTrans)
    val in1 = Input(new AddrTrans)
    val out0 = Decoupled(new AddrTrans)
    val out1 = Decoupled(new AddrTrans)
    val flush = Input(Bool())
    val csr_in = new CsrToMmuBundle
  })
  
  val s1 = Module(new MMUStage1)
  val s2 = Module(new MMUStage2)
  val tlb = Module(new TLB)

// 处理地址翻译逻辑
  s1.io.in0 := DontCare
  s1.io.in1 := DontCare
  tlb.io.csr := DontCare
  io.out0 <> s2.io.out0
  io.out1 <> s2.io.out1
  s1.io.tlb_interface0 <> tlb.io.s1_interface0
  s1.io.tlb_interface1 <> tlb.io.s1_interface1
  s2.io.tlb_interface0 <> tlb.io.s2_interface0
  s2.io.tlb_interface1 <> tlb.io.s2_interface1
  // 拼接物理地址
  val vseg_0 = s2.io.out0.bits.vaddr(31, 29)
  assert(!(io.csr_in.crmd.pg === 1.U && io.csr_in.crmd.da === 1.U))
  val pg_mode_0 = io.csr_in.crmd.pg & ~io.csr_in.crmd.da
  val da_mode_0 = ~io.csr_in.crmd.pg & io.csr_in.crmd.da

  val dmw0_hit_0 = io.csr_in.dmw0.vseg === vseg_0
  val dmw1_hit_0 = io.csr_in.dmw1.vseg === vseg_0
  val direct_map_0 = pg_mode_0 & (dmw0_hit_0 | dmw1_hit_0)
  val tlb_map_0 = pg_mode_0 & (~dmw0_hit_0 & ~dmw1_hit_0)
  io.out0.bits.paddr := (
    (Fill(ADDR_WIDTH, da_mode_0) & s2.io.out0.bits.vaddr) |
    (Fill(ADDR_WIDTH, dmw0_hit_0) & Cat(io.csr_in.dmw0.pseg, s2.io.out0.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, dmw1_hit_0) & Cat(io.csr_in.dmw1.pseg, s2.io.out0.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, tlb_map_0) & Cat(s2.io.out0.bits.ppn, s2.io.out0.bits.vaddr(28, 0)))
  )

  val vseg_1 = s2.io.out1.bits.vaddr(31, 29)
  assert(!(io.csr_in.crmd.pg === 1.U && io.csr_in.crmd.da === 1.U))
  val pg_mode_1 = io.csr_in.crmd.pg & ~io.csr_in.crmd.da
  val da_mode_1 = ~io.csr_in.crmd.pg & io.csr_in.crmd.da
  val dmw0_hit_1 = io.csr_in.dmw0.vseg === vseg_1
  val dmw1_hit_1 = io.csr_in.dmw1.vseg === vseg_1
  val direct_map_1 = pg_mode_1 & (dmw0_hit_1 | dmw1_hit_1)
  val tlb_map_1 = pg_mode_1 & (~dmw0_hit_1 & ~dmw1_hit_1)
  io.out1.bits.paddr := (
    (Fill(ADDR_WIDTH, da_mode_1) & s2.io.out1.bits.vaddr) |
    (Fill(ADDR_WIDTH, dmw0_hit_1) & Cat(io.csr_in.dmw0.pseg, s2.io.out1.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, dmw1_hit_1) & Cat(io.csr_in.dmw1.pseg, s2.io.out1.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, tlb_map_1) & Cat(s2.io.out1.bits.ppn, s2.io.out1.bits.vaddr(11, 0)))
  )

  MMUPipelineConnect(s1.io.out0, s2.io.in0, s1.io.out0.fire, io.flush)
  MMUPipelineConnect(s1.io.out1, s2.io.in1, s1.io.out1.fire, io.flush)
}

object GenMMU extends App {
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
    circt.stage.ChiselStage.emitSystemVerilogFile(new MMU(), args, firtoolOptions)
}