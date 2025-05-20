package core
import chisel3._
import chisel3.util._
import core.BpuConfig._

class BhtEntry extends Bundle {
  val history = UInt(HISTORY_WIDTH.W)
}
//                                          ||
// 2: pc = xxxxxxxx_xxxxxxxx_xxxxxxxx_xxxxxxxx
class BtbEntry extends Bundle {
  val target = UInt(32.W)
}

class BranchTrainInfo extends Bundle {
  val pc = UInt(32.W)
  val target = UInt(32.W)
  val taken = Bool()
  val valid = Bool()
}