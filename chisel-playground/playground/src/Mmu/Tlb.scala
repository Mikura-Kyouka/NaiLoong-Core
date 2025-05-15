package core
import chisel3._
import chisel3.util._
import MmuConfig._

class Tlb extends Module {
    val io = IO(new Bundle {
        val s0 = new tlb_s_datapacket()
        val s1 = new tlb_s_datapacket()

        val w_en = Input(Bool())
        val w_index = Input(UInt(log2Ceil(TLB_NUM).W))
        val w = Input(new tlb_bundle)

        val r_index = Input(UInt(log2Ceil(TLB_NUM).W))
        val r = Output(new tlb_bundle)

        val invtlb = Input(new tlb_invalid_bundle)
    })
    // 虚实地址转换
    val tlb = Reg(Vec(TLB_NUM, new tlb_bundle()))
    val hit0 = Wire(Vec(TLB_NUM, UInt(1.W)))
    val hit1 = Wire(Vec(TLB_NUM, UInt(1.W)))

    for(i <- 0 until TLB_NUM) {
        hit0(i) := (tlb(i).e.asBool) && 
                   ((tlb(i).g.asBool) || (tlb(i).asid === io.s0.asid)) &&
                   (tlb(i).vppn === io.s0.vppn)
        hit1(i) := (tlb(i).e.asBool) && 
                   ((tlb(i).g.asBool) || (tlb(i).asid === io.s1.asid)) &&
                   (tlb(i).vppn === io.s1.vppn)
    }

    val hit0_index = OHToUInt(hit0.asUInt)
    val hit1_index = OHToUInt(hit1.asUInt)

    io.s0.found := hit0.asUInt.orR
    io.s0.index := hit0_index
    io.s0.ppn := Mux(io.s0.va_bit12.asBool, tlb(hit0_index).ppn1, tlb(hit0_index).ppn0)
    io.s0.ps := PAGE_WIDTH.U
    io.s0.plv := Mux(io.s0.va_bit12.asBool, tlb(hit0_index).plv1, tlb(hit0_index).plv0)
    io.s0.mat := Mux(io.s0.va_bit12.asBool, tlb(hit0_index).mat1, tlb(hit0_index).mat0)
    io.s0.d := Mux(io.s0.va_bit12.asBool, tlb(hit0_index).d1, tlb(hit0_index).d0)
    io.s0.v := Mux(io.s0.va_bit12.asBool, tlb(hit0_index).v1, tlb(hit0_index).v0)

    io.s1.found := hit1.asUInt.orR
    io.s1.index := hit1_index
    io.s1.ppn := Mux(io.s1.va_bit12.asBool, tlb(hit1_index).ppn1, tlb(hit1_index).ppn0)
    io.s1.ps := PAGE_WIDTH.U
    io.s1.plv := Mux(io.s1.va_bit12.asBool, tlb(hit1_index).plv1, tlb(hit1_index).plv0)
    io.s1.mat := Mux(io.s1.va_bit12.asBool, tlb(hit1_index).mat1, tlb(hit1_index).mat0)
    io.s1.d := Mux(io.s1.va_bit12.asBool, tlb(hit1_index).d1, tlb(hit1_index).d0)
    io.s1.v := Mux(io.s1.va_bit12.asBool, tlb(hit1_index).v1, tlb(hit1_index).v0)

    dontTouch(tlb)

    // 读操作
    io.r := tlb(io.r_index)

    // 写操作
    when(io.w_en) {
        tlb(io.w_index) := io.w
    }

    // tlb invalid
    // TODO：暂无op的定义
    for(i <- 0 until TLB_NUM){
        when(io.invtlb.valid) {
            switch(io.invtlb.op) {
                is(0.U) { tlb(i).e := 0.U }
                is(1.U) { tlb(i).e := 0.U }
                is(2.U) {
                    when(tlb(i).g === 1.U) { tlb(i).e := 0.U }
                }
                is(3.U) {
                    when(tlb(i).g === 0.U) { tlb(i).e := 0.U }
                }
                is(4.U) {
                    when(tlb(i).g === 0.U && tlb(i).asid === io.invtlb.asid) { tlb(i).e := 0.U }
                }
                is(5.U) {
                    when(tlb(i).g === 0.U && tlb(i).asid === io.invtlb.asid && tlb(i).vppn === io.invtlb.va) { tlb(i).e := 0.U }
                }
                is(6.U) {
                    when((tlb(i).g === 0.U || tlb(i).asid === io.invtlb.asid) && tlb(i).vppn === io.invtlb.va) { tlb(i).e := 0.U }
                }
            }
        }
    }
}