package core

import chisel3._
import chisel3.util._

class IFU2IDU extends Bundle {
  val pc = Output(UInt(32.W))
  val inst = Output(UInt(32.W))
  val Valid = Output(Bool())
}

class IFU extends Module{
    val io = IO(new Bundle {
        val out   = Decoupled(Vec(4, new IFU2IDU))
        val axi   = new AXI
        val pcSel = Input(Bool())
        val flush = Input(Bool())
        val dnpc  = Input(UInt(32.W))
    })
    io.axi := DontCare
    /* Full AXI4 initialization */
    io.axi.arid := 0.U(4.W)
    io.axi.awid := 0.U(4.W)
    io.axi.awlen := 0.U(8.W)
    io.axi.awsize := 0.U(3.W)
    io.axi.awburst := 0.U(2.W)
    io.axi.wlast := true.B
    io.axi.awvalid := false.B
    io.axi.bready := false.B
    io.axi.wvalid := false.B
    io.axi.wdata := 0.U
    io.axi.wstrb := 0.U
    io.axi.awaddr := 0.U

    val pc = Module(new PC())
    val icache = Module(new PipelinedICache()(new ICacheConfig(totalSize = 4 * 16, ways = 1))) // Pipelined
    io.out.valid := icache.io.out.valid && !io.flush // TODO
    pc.io.PCSrc := io.pcSel
    pc.io.dnpc := io.dnpc
    pc.io.stall :=  ~icache.io.s1Fire
    // io.out.bits.pc := icache.io.out.bits.addr

    icache.io.axi <> io.axi
    icache.io.out.ready := io.out.ready
    icache.io.in.addr := pc.io.pc

    icache.io.flush := io.flush 
    icache.io.in.valid := io.out.ready && !io.flush // TODO
    // io.out.bits.inst := icache.io.out.bits.rdata
    io.out.bits := icache.io.out.bits
}


