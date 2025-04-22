// File: src/main/resources/FileLoggerBlackBox.v
module FileLoggerBlackBox(
  input        clk,
  input        en,
  input [31:0] pc,
  input  [7:0] queueType
);
  integer file;
  initial begin
    // “debug.txt” 会被输出到运行目录下
    file = $fopen("debug.txt", "w");
  end

  always @(posedge clk) begin
    if (en) begin
      // 每次 handshake 都输出一行
      $display(file, "queueType=%0d, pc=0x%08h", queueType, pc);
    end
  end
endmodule
