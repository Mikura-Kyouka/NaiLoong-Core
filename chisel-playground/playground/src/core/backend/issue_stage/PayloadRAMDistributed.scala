package core

import chisel3._
import chisel3.util._
import core.CSROpType.set
import core.IssueConfig.PHYS_REG_NUM


class PayloadRAMDistributed extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk    = Input(Clock())
    val raddr1 = Input(Vec(5, UInt(6.W)))
    val raddr2 = Input(Vec(5, UInt(6.W)))
    val rdata1 = Output(Vec(5, UInt(32.W)))
    val rdata2 = Output(Vec(5, UInt(32.W)))
    val wen    = Input(Vec(5, Bool()))
    val waddr  = Input(Vec(5, UInt(6.W)))
    val wdata  = Input(Vec(5, UInt(32.W)))
  })

  setInline("PayloadRAMDistributed.sv",
    s"""
    |module PayloadRAMDistributed (
    |  input  wire        clk,
    |  input  wire [5:0]  raddr1 [4:0],
    |  input  wire [5:0]  raddr2 [4:0],
    |  output wire [31:0] rdata1 [4:0],
    |  output wire [31:0] rdata2 [4:0],
    |  input  wire        wen    [4:0],
    |  input  wire [5:0]  waddr  [4:0],
    |  input  wire [31:0] wdata  [4:0]
    |);
    |
    |  (* ram_style = "distributed" *) reg [31:0] mem [0:63];
    |  integer i;
    |
    |  // 同步写
    |  always @(posedge clk) begin
    |    for (i = 0; i < 5; i = i + 1) begin
    |      if (wen[i]) begin
    |        mem[waddr[i]] <= wdata[i];
    |      end
    |    end
    |  end
    |
    |  // 组合读
    |  genvar j;
    |  generate
    |    for (j = 0; j < 5; j = j + 1) begin
    |      assign rdata1[j] = mem[raddr1[j]];
    |      assign rdata2[j] = mem[raddr2[j]];
    |    end
    |  endgenerate
    |
    |endmodule
    """.stripMargin)
}
