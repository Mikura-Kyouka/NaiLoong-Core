package core

import chisel3._
import chisel3.util._

class PCIO extends Bundle {
  val stall = Input(Bool()) // 当多周期处理器的没有到最后一步时，暂停pc
  val dnpc = Input(UInt(32.W))
  val pc = Output(UInt(32.W))
  val PCSrc = Input(Bool())
  val PCPredictTaken = Input(Bool())
  val nextPC = Output(UInt(32.W))
  //val refetch = Ouput(Bool())
}

class PC extends Module {
  val io = IO(new PCIO)
  val pcReg = RegInit("h1c000000".U(32.W)) // TODO
  // 00 04 08 0c, 10 14 18 1c // 20 
  val snpc = Wire(UInt(32.W))
  snpc := MuxLookup(pcReg(4, 0), (pcReg + 16.U))(
    Seq(
      0.U -> (pcReg + 16.U),
      4.U -> (pcReg + 16.U),
      8.U -> (pcReg + 16.U),
      12.U -> (pcReg + 16.U),
      16.U -> Cat((pcReg + 16.U)(31, 5), 0.U(5.W)),
      20.U -> Cat((pcReg + 16.U)(31, 5), 0.U(5.W)),
      24.U -> Cat((pcReg + 16.U)(31, 5), 0.U(5.W)),
      28.U -> Cat((pcReg + 16.U)(31, 5), 0.U(5.W))
    )
  )
  val predictTakenReg = RegInit(false.B)
  val predictTargetReg = RegInit("h00000000".U(32.W))

  when(!io.stall || io.PCSrc) {
    predictTakenReg := false.B
  }
  when(io.stall && !io.PCSrc && io.PCPredictTaken) {
    predictTakenReg := true.B
    predictTargetReg := io.dnpc
  }

  io.nextPC := snpc

  when(io.PCSrc) {
    pcReg := io.dnpc
    io.nextPC := io.dnpc
  }.elsewhen(io.stall) {
    pcReg := pcReg
    io.nextPC := pcReg
  }.elsewhen(io.PCPredictTaken) {
    pcReg := io.dnpc
    io.nextPC := io.dnpc
  }.elsewhen(predictTakenReg) {
    pcReg := predictTargetReg
    io.nextPC := predictTargetReg
  }.elsewhen(~io.stall){
    pcReg := snpc
    io.nextPC := snpc
  }
  io.pc := pcReg

}
