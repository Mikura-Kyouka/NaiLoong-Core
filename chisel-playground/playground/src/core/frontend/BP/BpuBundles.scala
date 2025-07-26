package core
import chisel3._
import chisel3.util._
import core.BpuConfig._

class BhtEntry extends Bundle {
  val history = UInt(HISTORY_WIDTH.W)
}
//                                          ||
// 2: pc = xxxxxxxx_xxxxxxxx_xxxxxxxx_xxxxxxxx
// class BtbEntry extends Bundle {
//   val target = UInt(32.W)
//   val isCall = Bool()
//   val isReturn = Bool()
// }

class BranchTrainInfo extends Bundle {
  val pc = UInt(32.W)
  val target = UInt(32.W)
  val taken = Bool()
  val valid = Bool()
  val isCall = Bool()
  val isReturn = Bool()
}