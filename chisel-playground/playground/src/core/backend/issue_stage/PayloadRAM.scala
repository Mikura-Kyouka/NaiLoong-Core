package core
import chisel3._
import chisel3.util._
import IssueConfig._

class PayloadRAM extends Module {
  val io = IO(new Bundle {
    val read = Vec(ISSUE_WIDTH, new payloadram_read_info)
    val write = Vec(5, new payloadram_write_info)
  })

  val pram = Reg(Vec(PHYS_REG_NUM, UInt(32.W)))
 
  // read
  io.read(0).pram_data1 := pram(io.read(0).src1 - 1.U)
  io.read(0).pram_data2 := pram(io.read(0).src2 - 1.U)
  io.read(1).pram_data1 := pram(io.read(1).src1 - 1.U)
  io.read(1).pram_data2 := pram(io.read(1).src2 - 1.U)
  io.read(2).pram_data1 := pram(io.read(2).src1 - 1.U)
  io.read(2).pram_data2 := pram(io.read(2).src2 - 1.U)
  io.read(3).pram_data1 := pram(io.read(3).src1 - 1.U)
  io.read(3).pram_data2 := pram(io.read(3).src2 - 1.U)
  io.read(4).pram_data1 := pram(io.read(4).src1 - 1.U)
  io.read(4).pram_data2 := pram(io.read(4).src2 - 1.U)

  // write
  for(i <- 0 until 5) {
    when(io.write(i).valid && io.write(i).dest.orR) {
      pram(io.write(i).dest - 1.U) := io.write(i).pram_data
    }
  }

  // val pram = Module(new PayloadRAMDistributed)

  // pram.io.clk := clock

  // for (i <- 0 until 5) {
  //   pram.io.wen(i)   := io.write(i).valid
  //   pram.io.waddr(i) := io.write(i).dest - 1.U
  //   pram.io.wdata(i) := io.write(i).pram_data
  // }

  // for (i <- 0 until 5) {
  //   pram.io.raddr1(i) := io.read(i).src1 - 1.U
  //   pram.io.raddr2(i) := io.read(i).src2 - 1.U

  //   io.read(i).pram_data1 := pram.io.rdata1(i)
  //   io.read(i).pram_data2 := pram.io.rdata2(i)
  // }
}

// class playloadram_write_info extends Bundle {
//   val dest = Input(UInt(PHYS_REG_BITS.W))
//   val pram_data = Input(new playloadram_info)
//   val valid = Input(Bool())
// }