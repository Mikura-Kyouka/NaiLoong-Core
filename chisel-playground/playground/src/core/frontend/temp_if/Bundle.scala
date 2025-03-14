import chisel3._
import chisel3.util._
class inst_info_ extends Bundle {
  val inst = UInt(32.W)
  val pc = UInt(32.W)
  val valid = Bool()
}