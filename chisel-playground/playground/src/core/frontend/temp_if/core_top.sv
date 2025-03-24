module core_top
#(
	parameter TLBNUM = 16
)
(
    input           aclk,
    input           aresetn,
    input    [ 7:0] intrpt, 
    //AXI interface 
    //read reqest
    output   [ 3:0] arid,
    output   [31:0] araddr,
    output   [ 7:0] arlen,
    output   [ 2:0] arsize,
    output   [ 1:0] arburst,
    output   [ 1:0] arlock,
    output   [ 3:0] arcache,
    output   [ 2:0] arprot,
    output          arvalid,
    input           arready,
    //read back
    input    [ 3:0] rid,
    input    [31:0] rdata,
    input    [ 1:0] rresp,
    input           rlast,
    input           rvalid,
    output          rready,
    //write request
    output   [ 3:0] awid,
    output   [31:0] awaddr,
    output   [ 7:0] awlen,
    output   [ 2:0] awsize,
    output   [ 1:0] awburst,
    output   [ 1:0] awlock,
    output   [ 3:0] awcache,
    output   [ 2:0] awprot,
    output          awvalid,
    input           awready,
    //write data
    output   [ 3:0] wid,
    output   [31:0] wdata,
    output   [ 3:0] wstrb,
    output          wlast,
    output          wvalid,
    input           wready,
    //write back
    input    [ 3:0] bid,
    input    [ 1:0] bresp,
    input           bvalid,
    output          bready,

    //debug
    input           break_point,
    input           infor_flag,
    input  [ 4:0]   reg_num,
    output          ws_valid,
    output [31:0]   rf_rdata,

    output [31:0] debug0_wb_pc,
    output [ 3:0] debug0_wb_rf_wen,
    output [ 4:0] debug0_wb_rf_wnum,
    output [31:0] debug0_wb_rf_wdata
);
reg         reset;
always @(posedge aclk) reset <= ~aresetn; 
//NOTE:在此处实例化生成的chisel顶层接口达到转接效果
Frontend fr(
    .clock            (aclk),           
    .reset            (reset),         
    .io_intrpt        (intrpt),            
    // AXI interface connections
    // read request
    .io_arid          (arid),
    .io_araddr        (araddr),
    .io_arlen         (arlen),
    .io_arsize        (arsize),
    .io_arburst       (arburst),
    .io_arlock        (arlock),
    .io_arcache       (arcache),
    .io_arprot        (arprot),
    .io_arvalid       (arvalid),
    .io_arready       (arready),
    // read response
    .io_rid           (rid),
    .io_rdata         (rdata),
    .io_rresp         (rresp),
    .io_rlast         (rlast),
    .io_rvalid        (rvalid),
    .io_rready        (rready),
    // write request
    .io_awid          (awid),
    .io_awaddr        (awaddr),
    .io_awlen         (awlen),
    .io_awsize        (awsize),
    .io_awburst       (awburst),
    .io_awlock        (awlock),
    .io_awcache       (awcache),
    .io_awprot        (awprot),
    .io_awvalid       (awvalid),
    .io_awready       (awready),
    // write data
    .io_wid           (wid),
    .io_wdata         (wdata),
    .io_wstrb         (wstrb),
    .io_wlast         (wlast),
    .io_wvalid        (wvalid),
    .io_wready        (wready),
    // write response
    .io_bid           (bid),
    .io_bresp         (bresp),
    .io_bvalid        (bvalid),
    .io_bready        (bready),
    // debug
    .io_break_point   (break_point),
    .io_infor_flag    (infor_flag),
    .io_reg_num       (reg_num),
    .io_ws_valid      (ws_valid),
    .io_rf_rdata      (rf_rdata),
    .io_debug0_wb_pc      (debug0_wb_pc),
    .io_debug0_wb_rf_wen  (debug0_wb_rf_wen),
    .io_debug0_wb_rf_wnum (debug0_wb_rf_wnum),
    .io_debug0_wb_rf_wdata(debug0_wb_rf_wdata),

    .io_debug1_wb_pc      (),
    .io_debug1_wb_rf_wen  (),
    .io_debug1_wb_rf_wnum (),
    .io_debug1_wb_rf_wdata()
);


endmodule
