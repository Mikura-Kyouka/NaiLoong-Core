package core

import chisel3._
import chisel3.util._

class DataArray extends Module {
  val io = IO(new Bundle {
    val addra = Input(UInt(16.W))
    val clka = Input(Clock())
    val dina = Input(UInt(32.W))
    val douta = Output(UInt(32.W))
    val wea = Input(Bool())

    val addrb = Input(UInt(16.W))
    val clkb = Input(Clock())
    val dinb = Input(UInt(32.W))
    val doutb = Output(UInt(32.W))
    val web = Input(Bool())
  })
  
  val mem = SyncReadMem(65536, UInt(32.W))

  when(io.wea) {
    mem.write(io.addra, io.dina)
  }
  io.douta := mem.read(io.addra, io.clka)

  when(io.web) {
    mem.write(io.addrb, io.dinb)
  }
  io.doutb := mem.read(io.addrb, io.clkb)
}
