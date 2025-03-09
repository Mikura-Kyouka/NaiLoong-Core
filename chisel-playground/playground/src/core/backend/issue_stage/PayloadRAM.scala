import chisel3._
import chisel3.util._
import IssueConfig._

class PayloadRAM extends Module {
  val io = IO(new Bundle {
    val read = Vec(ISSUE_WIDTH, new payloadram_read_info)
    val write = new payloadram_write_info
  })

  val pram = Mem(PHYS_REG_NUM, UInt(32.W))
 
  // read
  io.read(0).pram_data1 := pram(io.read(0).src1)
  io.read(0).pram_data2 := pram(io.read(0).src2)
  io.read(1).pram_data1 := pram(io.read(1).src1)
  io.read(1).pram_data2 := pram(io.read(1).src2)
  io.read(2).pram_data1 := pram(io.read(2).src1)
  io.read(2).pram_data2 := pram(io.read(2).src2)
  io.read(3).pram_data1 := pram(io.read(3).src1)
  io.read(3).pram_data2 := pram(io.read(3).src2)

  // write
  when(io.write.valid) {
    pram(io.write.dest) := io.write.pram_data
  }
}

// class playloadram_write_info extends Bundle {
//   val dest = Input(UInt(PHYS_REG_BITS.W))
//   val pram_data = Input(new playloadram_info)
//   val valid = Input(Bool())
// }