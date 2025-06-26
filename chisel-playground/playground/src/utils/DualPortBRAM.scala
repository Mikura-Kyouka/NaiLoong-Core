package core
import chisel3._
import chisel3.util._

class DualPortBRAMIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val clka  = Input(Clock())
  val wea   = Input(Bool())
  val addra = Input(UInt(addrWidth.W))
  val dina  = Input(UInt(dataWidth.W))

  val addrb = Input(UInt(addrWidth.W))
  val doutb = Output(UInt(dataWidth.W))
}

class DualPortBRAM(val addrWidth: Int, val dataWidth: Int) extends Module {
  val io = IO(new DualPortBRAMIO(addrWidth, dataWidth))

  if (GenCtrl.USE_SIMU) {
    val mem = SyncReadMem(1 << addrWidth, UInt(dataWidth.W))

    withClock(io.clka) {
      val doutaReg = RegInit(0.U(dataWidth.W))
      when(io.wea) {
        mem.write(io.addra, io.dina)
      }
    }

    withClock(io.clka) {
      val doutbReg = RegInit(0.U(dataWidth.W))
      doutbReg := mem.read(io.addrb, true.B)
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
    | module BlackBoxDualPortBRAM #(
    |     parameter ADDR_WIDTH = 64,                       // Specify RAM data width
    |     parameter DATA_WIDTH = 512                      // Specify RAM depth (number of entries)
    |   ) (
    |     input  wire [ADDR_WIDTH-1:0] addra, // Write address bus, width determined from RAM_DEPTH
    |     input  wire [ADDR_WIDTH-1:0] addrb, // Read address bus, width determined from RAM_DEPTH
    |     input  wire [DATA_WIDTH-1:0] dina,          // RAM input data
    |     input  wire clka,                          // Clock
    |     input  wire wea,                           // Write enable
    |     output wire  [DATA_WIDTH-1:0] doutb         // RAM output data
    |   );
    |   (*ram_style="block"*)
    |     reg [DATA_WIDTH-1:0] BRAM [(1<<ADDR_WIDTH)-1:0];
    |     reg [ADDR_WIDTH-1:0] addr_r;
    |     reg is_collision;
    |     reg [DATA_WIDTH-1:0] collison_data;
    |
    |   generate
    |       integer ram_index;
    |       initial
    |         for (ram_index = 0; ram_index < (1<<ADDR_WIDTH); ram_index = ram_index + 1)
    |           BRAM[ram_index] = {DATA_WIDTH{1'b0}};
    |   endgenerate
    |
    |     always @(posedge clka) begin
    |         addr_r <= addrb;
    |         is_collision <= (addra == addrb && wea);
    |         collison_data <= dina;
    |         if (wea) BRAM[addra] <= dina;
    |     end
    |
    |     assign doutb = is_collision == 1'b1 ? collison_data : BRAM[addr_r];
    |   endmodule
    """.stripMargin)
}

