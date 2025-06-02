package core
import chisel3._
import MmuConfig._
import Ecode._

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

    val s2_interface0 = Flipped(new Stage2Interface)
    val s2_interface1 = Flipped(new Stage2Interface)

    val from_csr = new CsrToMmuBundle
    val to_csr = new MmuToCsrBundle

    val wen = Input(Bool())
    val w_index = Input(UInt(log2Ceil(TLB_NUM).W))
    val w = Input(new TlbBundle)

    val r_index = Input(UInt(log2Ceil(TLB_NUM).W))
    val r = Output(new TlbBundle)

    val tlb_inst = Input(new TlbInstBundle)
  })

  val tlb = Reg(Vec(TLB_NUM, new TlbBundle))
  io.to_csr := DontCare

// 处理 Stage 1 : 判断是否命中 TLB
  val hit0 = Wire(Vec(TLB_NUM, Bool()))
  val hit1 = Wire(Vec(TLB_NUM, Bool()))

  // 设计实战 P51
  for (i <- 0 until TLB_NUM) {
    hit0(i) := (tlb(i).e.asBool) && 
               ((tlb(i).g.asBool) || (tlb(i).asid === io.from_csr.asid.asid)) &&
               (tlb(i).vppn === io.s1_interface0.vppn)
  }
  for (i <- 0 until TLB_NUM) {
    hit1(i) := (tlb(i).e.asBool) && 
               ((tlb(i).g.asBool) || (tlb(i).asid === io.from_csr.asid.asid)) &&
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

// tlb write
  when(io.wen) {
    tlb(io.w_index) := io.w
  }
// tlb read
  io.r := tlb(io.r_index)

// 处理tlb指令
  val is_tlb_refill = io.from_csr.estat.ecode === "h3F".U // TLB refill exception code
  val tlb_refill_idx = RegInit(0.U(log2Ceil(TLB_NUM).W))
  when(io.tlb_inst.en) {
    switch(io.tlb_inst.inst_type) {
      is(TlbOp.srch) {  // 暂时写死4kb
        val temp_hit_vec = Wire(Vec(TLB_NUM, Bool()))
        for (i <- 0 until TLB_NUM) {
          temp_hit_vec(i) := (tlb(i).e.asBool) && 
                             ((tlb(i).g.asBool) || (tlb(i).asid === io.from_csr.asid.asid)) &&
                             (tlb(i).vppn === io.from_csr.tlbehi.vppn)
        }
        val temp_hit_index = OHToUInt(temp_hit_vec.asUInt)
        io.to_csr.wen := true.B
        io.to_csr.inst_type := TlbOp.srch
        io.to_csr.tlb_hit := temp_hit_vec.asUInt.orR
        io.to_csr.tlb_idx := temp_hit_index
        io.to_csr.tlb_entry := tlb(temp_hit_index)
      }
      is(TlbOp.rd) {
        io.to_csr.wen := true.B
        io.to_csr.inst_type := TlbOp.rd
        io.to_csr.tlb_entry := tlb(io.from_csr.tlbidx.idx)
      }
      is(TlbOp.wr) {
        tlb(io.from_csr.tlbidx.idx).ps := io.from_csr.tlbidx.ps
        tlb(io.from_csr.tlbidx.idx).e := !io.from_csr.tlbidx.ne.asBool || is_tlb_refill
        tlb(io.from_csr.tlbidx.idx).vppn := io.from_csr.tlbehi.vppn
        tlb(io.from_csr.tlbidx.idx).g := io.from_csr.tlbelo0.g.asBool && io.from_csr.tlbelo1.g.asBool
        tlb(io.from_csr.tlbidx.idx).asid := io.from_csr.asid.asid

        tlb(io.from_csr.tlbidx.idx).ppn0 := io.from_csr.tlbelo0.ppn
        tlb(io.from_csr.tlbidx.idx).plv0 := io.from_csr.tlbelo0.plv
        tlb(io.from_csr.tlbidx.idx).mat0 := io.from_csr.tlbelo0.mat
        tlb(io.from_csr.tlbidx.idx).d0 := io.from_csr.tlbelo0.d
        tlb(io.from_csr.tlbidx.idx).v0 := io.from_csr.tlbelo0.v

        tlb(io.from_csr.tlbidx.idx).ppn1 := io.from_csr.tlbelo1.ppn
        tlb(io.from_csr.tlbidx.idx).plv1 := io.from_csr.tlbelo1.plv
        tlb(io.from_csr.tlbidx.idx).mat1 := io.from_csr.tlbelo1.mat
        tlb(io.from_csr.tlbidx.idx).d1 := io.from_csr.tlbelo1.d
        tlb(io.from_csr.tlbidx.idx).v1 := io.from_csr.tlbelo1.v
      }
      is(TlbOp.fill) {
        tlb_refill_idx := tlb_refill_idx + 1.U

        tlb(tlb_refill_idx).ps := io.from_csr.tlbidx.ps
        tlb(tlb_refill_idx).e := !io.from_csr.tlbidx.ne.asBool || is_tlb_refill
        tlb(tlb_refill_idx).vppn := io.from_csr.tlbehi.vppn
        tlb(tlb_refill_idx).g := io.from_csr.tlbelo0.g.asBool && io.from_csr.tlbelo1.g.asBool
        tlb(tlb_refill_idx).asid := io.from_csr.asid.asid

        tlb(tlb_refill_idx).ppn0 := io.from_csr.tlbelo0.ppn
        tlb(tlb_refill_idx).plv0 := io.from_csr.tlbelo0.plv
        tlb(tlb_refill_idx).mat0 := io.from_csr.tlbelo0.mat
        tlb(tlb_refill_idx).d0 := io.from_csr.tlbelo0.d
        tlb(tlb_refill_idx).v0 := io.from_csr.tlbelo0.v

        tlb(tlb_refill_idx).ppn1 := io.from_csr.tlbelo1.ppn
        tlb(tlb_refill_idx).plv1 := io.from_csr.tlbelo1.plv
        tlb(tlb_refill_idx).mat1 := io.from_csr.tlbelo1.mat
        tlb(tlb_refill_idx).d1 := io.from_csr.tlbelo1.d
        tlb(tlb_refill_idx).v1 := io.from_csr.tlbelo1.v
      }
      is(TlbOp.inv) {
        for(i <- 0 until TLB_NUM) {
          switch(io.tlb_inst.op) {
            is(0.U) { tlb(i).e := 0.U } // 全部清除
            is(1.U) { tlb(i).e := 0.U } // 全部清除
            is(2.U) { // 全局清除
              when(tlb(i).g === 1.U) { tlb(i).e := 0.U }
            }
            is(3.U) { // 非全局清除
              when(tlb(i).g === 0.U) { tlb(i).e := 0.U }
            }
            is(4.U) { // 按 ASID 清除
              when(tlb(i).g === 0.U && tlb(i).asid === io.tlb_inst.asid) { tlb(i).e := 0.U }
            }
            is(5.U) {
              when(tlb(i).g === 0.U && tlb(i).asid === io.tlb_inst.asid && tlb(i).vppn === io.tlb_inst.va(31, 13)) { tlb(i).e := 0.U }
            }
            is(6.U) {
              when((tlb(i).g === 0.U || tlb(i).asid === io.tlb_inst.asid) && tlb(i).vppn === io.tlb_inst.va(31, 13)) { tlb(i).e := 0.U }
            }
          }
        }
      }
    }
  }
}

class MMU extends Module {
  import chisel3.util._
  val io = IO(new Bundle {
    val in0 = Input(new AddrTrans)
    val in1 = Input(new AddrTrans)
    val out0 = Decoupled(new AddrTrans)
    val out1 = Decoupled(new AddrTrans)
    val excp0 = Output(new ExceptionBundle)
    val excp1 = Output(new ExceptionBundle)
    val flush = Input(Bool())

    val from_csr = new CsrToMmuBundle
    val to_csr = new MmuToCsrBundle

    val w = Input(new TlbBundle)
    val wen = Input(Bool())
    val w_index = Input(UInt(log2Ceil(TLB_NUM).W))
    val r_index = Input(UInt(log2Ceil(TLB_NUM).W))
    val r = Output(new TlbBundle)
    
    val tlb_inst = Input(new TlbInstBundle)
  })
  
  val s1 = Module(new MMUStage1)
  val s2 = Module(new MMUStage2)
  val tlb = Module(new TLB)

  tlb.io.tlb_inst <> io.tlb_inst
  tlb.io.to_csr <> io.to_csr

// tlb 读写
  tlb.io.w := io.w
  tlb.io.wen := io.wen
  tlb.io.w_index := io.w_index
  tlb.io.r_index := io.r_index
  io.r := tlb.io.r

// 处理地址翻译逻辑
  s1.io.in0 := io.in0
  s1.io.in1 := io.in1
  tlb.io.from_csr := io.from_csr
  io.out0 <> s2.io.out0
  io.out1 <> s2.io.out1
  s1.io.tlb_interface0 <> tlb.io.s1_interface0
  s1.io.tlb_interface1 <> tlb.io.s1_interface1
  s2.io.tlb_interface0 <> tlb.io.s2_interface0
  s2.io.tlb_interface1 <> tlb.io.s2_interface1
  // 拼接物理地址
  val vseg_0 = s2.io.out0.bits.vaddr(31, 29)
  assert(!(io.from_csr.crmd.pg === 1.U && io.from_csr.crmd.da === 1.U))
  val pg_mode_0 = io.from_csr.crmd.pg & ~io.from_csr.crmd.da
  val da_mode_0 = ~io.from_csr.crmd.pg & io.from_csr.crmd.da

  val dmw0_hit_0 = io.from_csr.dmw0.vseg === vseg_0
  val dmw1_hit_0 = io.from_csr.dmw1.vseg === vseg_0
  val direct_map_0 = pg_mode_0 & (dmw0_hit_0 | dmw1_hit_0)
  val tlb_map_0 = pg_mode_0 & (~dmw0_hit_0 & ~dmw1_hit_0)
  io.out0.bits.paddr := (
    (Fill(ADDR_WIDTH, da_mode_0) & s2.io.out0.bits.vaddr) |
    (Fill(ADDR_WIDTH, dmw0_hit_0) & Cat(io.from_csr.dmw0.pseg, s2.io.out0.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, dmw1_hit_0) & Cat(io.from_csr.dmw1.pseg, s2.io.out0.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, tlb_map_0) & Cat(s2.io.out0.bits.ppn, s2.io.out0.bits.vaddr(11, 0)))
  )

  val vseg_1 = s2.io.out1.bits.vaddr(31, 29)
  assert(!(io.from_csr.crmd.pg === 1.U && io.from_csr.crmd.da === 1.U))
  val pg_mode_1 = io.from_csr.crmd.pg & ~io.from_csr.crmd.da
  val da_mode_1 = ~io.from_csr.crmd.pg & io.from_csr.crmd.da
  val dmw0_hit_1 = io.from_csr.dmw0.vseg === vseg_1
  val dmw1_hit_1 = io.from_csr.dmw1.vseg === vseg_1
  val direct_map_1 = pg_mode_1 & (dmw0_hit_1 | dmw1_hit_1)
  val tlb_map_1 = pg_mode_1 & (~dmw0_hit_1 & ~dmw1_hit_1)
  io.out1.bits.paddr := (
    (Fill(ADDR_WIDTH, da_mode_1) & s2.io.out1.bits.vaddr) |
    (Fill(ADDR_WIDTH, dmw0_hit_1) & Cat(io.from_csr.dmw0.pseg, s2.io.out1.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, dmw1_hit_1) & Cat(io.from_csr.dmw1.pseg, s2.io.out1.bits.vaddr(28, 0))) |
    (Fill(ADDR_WIDTH, tlb_map_1) & Cat(s2.io.out1.bits.ppn, s2.io.out1.bits.vaddr(11, 0)))
  )

  MMUPipelineConnect(s1.io.out0, s2.io.in0, s1.io.out0.fire, io.flush)
  MMUPipelineConnect(s1.io.out1, s2.io.in1, s1.io.out1.fire, io.flush)

// 判断例外
  // 赋个初始值
  io.excp0 := 0.U.asTypeOf(new ExceptionBundle)
  io.excp1 := 0.U.asTypeOf(new ExceptionBundle)
  when(!io.out0.bits.found.asBool) {
    io.excp0.en := true.B
    io.excp0.ecode := Ecode.tlbr
  }
  when(!io.out0.bits.v) {
    io.excp0.en := true.B
    io.excp0.ecode := io.out0.bits.mem_type
  }
  when(io.from_csr.crmd.plv > io.out0.bits.plv) {
    io.excp0.en := true.B
    io.excp0.ecode := Ecode.ppi
  }
  when(io.out0.bits.mem_type === MemType.store && !io.out0.bits.d.asBool) {
    io.excp0.en := true.B
    io.excp0.ecode := Ecode.pme
  }

  when(!io.out1.bits.found.asBool) {
    io.excp1.en := true.B
    io.excp1.ecode := Ecode.tlbr
  }
  when(!io.out1.bits.v) {
    io.excp1.en := true.B
    io.excp1.ecode := io.out1.bits.mem_type
  }
  when(io.from_csr.crmd.plv > io.out1.bits.plv) {
    io.excp1.en := true.B
    io.excp1.ecode := Ecode.ppi
  }
  when(io.out1.bits.mem_type === MemType.store && !io.out1.bits.d.asBool) {
    io.excp1.en := true.B
    io.excp1.ecode := Ecode.pme
  }
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