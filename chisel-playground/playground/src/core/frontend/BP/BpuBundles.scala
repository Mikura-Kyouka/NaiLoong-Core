package core
import chisel3._
import chisel3.util._
import core.BpuConfig._

class BhtEntry extends Bundle {
  val history = UInt(HISTORY_WIDTH.W)
  val valid = Bool()
}
object BhtEntry {
  def width: Int = (new BhtEntry).getWidth  // = HISTORY_WIDTH + 1
}
//                                          ||
// 2: pc = xxxxxxxx_xxxxxxxx_xxxxxxxx_xxxxxxxx
class BtbEntry extends Bundle {
  val target = UInt(32.W)
  val isCall = Bool()
  val isReturn = Bool()
  val valid = Bool()
}
object BtbEntry {
  def width: Int = (new BtbEntry).getWidth
}

class BranchTrainInfo extends Bundle {
  val pc = UInt(32.W)
  val target = UInt(32.W)
  val taken = Bool()
  val valid = Bool()
  val isCall = Bool()
  val isReturn = Bool()
}