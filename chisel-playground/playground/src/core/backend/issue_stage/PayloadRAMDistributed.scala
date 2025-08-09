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
    | module PayloadRAMDistributed (
    |   input  wire        clk,
    |   input  wire [5:0]  raddr1_0, raddr1_1, raddr1_2, raddr1_3, raddr1_4,
    |   input  wire [5:0]  raddr2_0, raddr2_1, raddr2_2, raddr2_3, raddr2_4,
    |   output wire [31:0] rdata1_0, rdata1_1, rdata1_2, rdata1_3, rdata1_4,
    |   output wire [31:0] rdata2_0, rdata2_1, rdata2_2, rdata2_3, rdata2_4,
    |   input  wire        wen_0, wen_1, wen_2, wen_3, wen_4,
    |   input  wire [5:0]  waddr_0, waddr_1, waddr_2, waddr_3, waddr_4,
    |   input  wire [31:0] wdata_0, wdata_1, wdata_2, wdata_3, wdata_4
    | );
    |
    |   // 把扁平端口收集成数组，方便后续循环
    |   wire [5:0] raddr1 [0:4] = '{raddr1_0, raddr1_1, raddr1_2, raddr1_3, raddr1_4};
    |   wire [5:0] raddr2 [0:4] = '{raddr2_0, raddr2_1, raddr2_2, raddr2_3, raddr2_4};
    |   wire       wen    [0:4] = '{wen_0,    wen_1,    wen_2,    wen_3,    wen_4};
    |   wire [5:0] waddr  [0:4] = '{waddr_0,  waddr_1,  waddr_2,  waddr_3,  waddr_4};
    |   wire [31:0] wdata [0:4] = '{wdata_0,  wdata_1,  wdata_2,  wdata_3,  wdata_4};
    |   reg  [31:0] rdata1 [0:4];
    |   reg  [31:0] rdata2 [0:4];
    |
    |   (* ram_style = "distributed" *) reg [31:0] mem [0:63];
    |   integer i;
    |
    |   initial begin
    |     for (i = 0; i < 64; i = i + 1)
    |       mem[i] = 32'b0;
    |   end
    |
    |   always @(posedge clk) begin
    |     for (i = 0; i < 5; i = i + 1) begin
    |       if (wen[i]) begin
    |         mem[waddr[i]] <= wdata[i];
    |       end
    |     end
    |   end
    |
    |   always @(*) begin
    |     for (i = 0; i < 5; i = i + 1) begin
    |       rdata1[i] = mem[raddr1[i]];
    |       rdata2[i] = mem[raddr2[i]];
    |     end
    |   end
    |
    |   // 把数组输出再拆回扁平信号
    |   assign {rdata1_0, rdata1_1, rdata1_2, rdata1_3, rdata1_4} =
    |         {rdata1[0], rdata1[1], rdata1[2], rdata1[3], rdata1[4]};
    |   assign {rdata2_0, rdata2_1, rdata2_2, rdata2_3, rdata2_4} =
    |         {rdata2[0], rdata2[1], rdata2[2], rdata2[3], rdata2[4]};
    |
    |endmodule
    """.stripMargin)
}
