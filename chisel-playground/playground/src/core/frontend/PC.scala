package core

import chisel3._
import chisel3.util._

class PCIO extends Bundle {
  val stall = Input(Bool()) // 当多周期处理器的没有到最后一步时，暂停pc
  val dnpc = Input(UInt(32.W))
  val pc = Output(UInt(32.W))
  val PCSrc = Input(Bool())
  //val refetch = Ouput(Bool())
}

class PC extends Module {
  val io = IO(new PCIO)
  val pcReg = RegInit("h1c000000".U(32.W)) // TODO
  val snpc = Cat((pcReg + 16.U)(31, 4), 0.U(4.W))
  when(io.PCSrc) {
    pcReg := io.dnpc
  }.elsewhen(~io.stall){
    pcReg := snpc
  }
  io.pc := pcReg

}
