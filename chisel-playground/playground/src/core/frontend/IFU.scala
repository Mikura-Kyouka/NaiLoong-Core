package core

import chisel3._
import chisel3.util._

class IFU2IDU extends Bundle {
  val pc = Output(UInt(32.W))
  val inst = Output(UInt(32.W))
}

class IFU extends Module{
    val io = IO(new Bundle {
        val out   = Vec(4, Decoupled(new IFU2IDU))
        val axi   = new AXI
        val flush = Input(Bool())
        val dnpc  = Input(UInt(32.W))
    })
   val icache = Module(new ICache()(new ICacheConfig(totalSize = 4 * 16, ways = 1))) // Pipelined
  
}

