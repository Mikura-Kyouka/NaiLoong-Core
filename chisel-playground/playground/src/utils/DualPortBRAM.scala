package core
import chisel3._
import chisel3.util._

class DualPortBRAMIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val clka  = Input(Clock())
  val ena   = Input(Bool())
  val wea   = Input(Bool())
  val addra = Input(UInt(addrWidth.W))
  val dina  = Input(UInt(dataWidth.W))
  val douta = Output(UInt(dataWidth.W))

  val clkb  = Input(Clock())
  val enb   = Input(Bool())
  val web   = Input(Bool())
  val addrb = Input(UInt(addrWidth.W))
  val dinb  = Input(UInt(dataWidth.W))
  val doutb = Output(UInt(dataWidth.W))
}

class DualPortBRAM(val addrWidth: Int, val dataWidth: Int) extends Module {
  val io = IO(new DualPortBRAMIO(addrWidth, dataWidth))

  if (GenCtrl.USE_SIMU) {
    val mem = SyncReadMem(1 << addrWidth, UInt(dataWidth.W))

    withClock(io.clka) {
      val doutaReg = RegInit(0.U(dataWidth.W))
      when(io.ena) {
        when(io.wea) {
          mem.write(io.addra, io.dina)
        }
        doutaReg := mem.read(io.addra, !io.wea)
      }
      io.douta := doutaReg
    }

    withClock(io.clkb) {
      val doutbReg = RegInit(0.U(dataWidth.W))
      when(io.enb) {
        when(io.web) {
          mem.write(io.addrb, io.dinb)
        }
        doutbReg := mem.read(io.addrb, !io.web)
      }
      io.doutb := doutbReg
    }

  } else {
    val bram = Module(new BlackBoxDualPortBRAM(addrWidth, dataWidth))
    bram.io <> io
  }
}

import chisel3.experimental._

class BlackBoxDualPortBRAM(val addrWidth: Int, val dataWidth: Int) extends BlackBox(Map(
  "ADDR_WIDTH" -> addrWidth,
  "DATA_WIDTH" -> dataWidth
)) with HasBlackBoxInline {

  val io = IO(new DualPortBRAMIO(addrWidth, dataWidth))

  // Verilog module inline definition
  val module = "BlackBoxDualPortBRAM.sv"
    setInline(module,
    """
    |module BlackBoxDualPortBRAM #(
    |    parameter ADDR_WIDTH = 10,
    |    parameter DATA_WIDTH = 32
    |) (
    |    input  wire [ADDR_WIDTH-1:0] addra,
    |    input  wire clka,
    |    input  wire [DATA_WIDTH-1:0] dina,
    |    output reg  [DATA_WIDTH-1:0] douta,
    |    input  wire ena,
    |    input  wire wea,
    |
    |    input  wire [ADDR_WIDTH-1:0] addrb,
    |    input  wire clkb,
    |    input  wire [DATA_WIDTH-1:0] dinb,
    |    output reg  [DATA_WIDTH-1:0] doutb,
    |    input  wire enb,
    |    input  wire web
    |);
    |
    |  (* ram_style = "block" *) 
    |  reg [DATA_WIDTH-1:0] mem [0:(1<<ADDR_WIDTH)-1];
    |
    |  integer i;
    |  initial begin
    |    for (i = 0; i < (1 << ADDR_WIDTH); i = i + 1) begin
    |      mem[i] = 0;
    |    end
    |  end
    |
    |  always @(posedge clka) begin
    |    if (ena) begin
    |      if (wea) mem[addra] <= dina;
    |      douta <= mem[addra];
    |    end
    |  end
    |
    |  always @(posedge clkb) begin
    |    if (enb) begin
    |      if (web) mem[addrb] <= dinb;
    |      doutb <= mem[addrb];
    |    end
    |  end
    |
    |endmodule
    """.stripMargin)
}

