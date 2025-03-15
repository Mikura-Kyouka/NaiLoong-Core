import chisel3._
import chisel3.util._
class inst_info_ extends Bundle {
  val inst = UInt(32.W)
  val pc = UInt(32.W)
  val valid = Bool()
}

class if_to_id_info extends Bundle {
  val inst0 = new inst_info_
  val inst1 = new inst_info_
  val inst2 = new inst_info_
  val inst3 = new inst_info_
}