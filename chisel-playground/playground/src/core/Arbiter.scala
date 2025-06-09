package core
import chisel3._
import chisel3.util._

class Arb extends Module {
  val io = IO(new Bundle {
    val ifu = Flipped(new AXI)
    val lsu = Flipped(new AXI)
    val out = new AXI
  })

  /*   00         01        10            11     */
  val s_idle :: s_ifu :: s_lsu_write :: s_lsu_read :: Nil = Enum(4)
  val state = RegInit(s_idle)

  // state := MuxLookup(state, s_idle)(
  //   List(
  //     // s_idle -> Mux(
  //     //   io.ifu.arvalid,
  //     //   s_ifu,
  //     //   Mux(
  //     //     io.lsu.awvalid & io.lsu.wvalid,
  //     //     s_lsu_write,
  //     //     Mux(io.lsu.arvalid, s_lsu_read, s_idle)
  //     //   )
  //     // ),
  //     s_idle -> Mux(
  //       io.lsu.awvalid & io.lsu.wvalid,
  //       s_lsu_write,
  //       Mux(
  //         io.lsu.arvalid,
  //         s_lsu_read,
  //         Mux(io.ifu.arvalid, s_ifu, s_idle)
  //       )
  //     ),
  //     s_ifu -> Mux(io.ifu.rlast, s_idle, s_ifu),
  //     s_lsu_write -> Mux(
  //       io.lsu.bvalid & io.lsu.bready,
  //       s_idle,
  //       s_lsu_write
  //     ),
  //     s_lsu_read -> Mux(
  //       io.lsu.rvalid & io.lsu.rready & io.lsu.rlast,
  //       s_idle,
  //       s_lsu_read
  //     )
  //   )
  // )

  switch(state) {
    is(s_idle) {
      when(io.lsu.awvalid) {
        state := s_lsu_write
      }.elsewhen(io.lsu.arvalid) {
        state := s_lsu_read
      }.elsewhen(io.ifu.arvalid) {
        state := s_ifu
      }
    }
    is(s_ifu) {
      when(io.out.rvalid && io.out.rready && io.out.rlast) {
        state := s_idle
      }
    }
    is(s_lsu_write) {
      when(io.out.bvalid && io.out.bready) {
        state := s_idle
      }
    }
    is(s_lsu_read) {
      when(io.out.rvalid && io.out.rready && io.out.rlast) {
        state := s_idle
      }
    }
  }

  /* Read transection */
  io.out.arid := 0.U
  io.out.arlen := 0.U
  io.out.arsize := "b010".U // 32 bits
  io.out.arburst := "b01".U
  /* Write transection */
  io.out.awid := 0.U
  io.out.awlen := 0.U
  io.out.awsize := "b010".U // 32 bits
  io.out.awburst := "b01".U
  io.out.wlast := true.B

  io.ifu.arready := 0.U
  io.ifu.rvalid := 0.U
  io.ifu.rdata := 0.U
  io.ifu.rresp := 0.U
  io.ifu.rlast := true.B

  io.ifu.awready := 0.U
  io.ifu.wready := 0.U
  io.ifu.bvalid := 0.U

  io.ifu.rid := 0.U
  io.ifu.bresp := 0.U
  io.ifu.bid := 0.U

  io.lsu.arready := 0.U
  io.lsu.rvalid := 0.U
  io.lsu.rdata := 0.U

  io.lsu.rresp := 0.U
  io.lsu.rlast := true.B
  io.lsu.rid := 0.U

  io.lsu.bid := 0.U

  io.lsu.awready := 0.U
  io.lsu.wready := 0.U
  io.lsu.bvalid := 0.U
  io.lsu.bresp := 0.U
  io.out.arvalid := 0.U
  io.out.araddr := 0.U
  io.out.rready := 0.U
  io.out.awvalid := 0.U
  io.out.awaddr := 0.U
  io.out.wvalid := 0.U
  io.out.wdata := 0.U
  io.out.wstrb := 0.U
  io.out.bready := 0.U

  when(state === s_ifu) {
    io.out.arvalid := io.ifu.arvalid
    io.out.araddr := io.ifu.araddr
    io.out.arlen := io.ifu.arlen

    io.out.arsize := io.ifu.arsize
    io.out.arburst := io.ifu.arburst
    io.out.rready := io.ifu.rready

    io.ifu.arready := io.out.arready
    io.ifu.rvalid := io.out.rvalid
    io.ifu.rlast := io.out.rlast
  }.elsewhen(state === s_lsu_read) {
    io.out.arvalid := io.lsu.arvalid
    io.out.araddr := io.lsu.araddr
    io.out.arsize := io.lsu.arsize //
    io.out.arlen := io.lsu.arlen 
    io.out.arburst := io.lsu.arburst
    io.out.rready := io.lsu.rready

    io.lsu.arready := io.out.arready
    io.lsu.rvalid := io.out.rvalid
    io.lsu.rlast := io.out.rlast
    io.lsu.rid := io.out.rid 
  }.elsewhen(state === s_lsu_write) {
    io.out.awvalid := io.lsu.awvalid
    io.out.wvalid := io.lsu.wvalid
    io.out.awaddr := io.lsu.awaddr
    io.out.awsize := io.lsu.awsize //
    io.out.awlen := io.lsu.awlen
    io.out.awburst := io.lsu.awburst
    io.out.wdata := io.lsu.wdata
    io.out.wstrb := io.lsu.wstrb
    io.out.bready := io.lsu.bready

    io.lsu.awready := io.out.awready
    io.lsu.wready := io.out.wready
    io.lsu.bvalid := io.out.bvalid
    io.lsu.bresp := io.out.bresp
  }.otherwise {
    io.out.araddr := 0.U
    io.out.arvalid := false.B
    io.out.rready := false.B
    io.out.bready := false.B

    io.out.awvalid := false.B
    io.out.wvalid := false.B

    io.ifu.arready := false.B
    io.ifu.rvalid := false.B
    io.ifu.bvalid := false.B
    io.lsu.arready := false.B
    io.lsu.rvalid := false.B
    io.lsu.awready := false.B
    io.lsu.wready := false.B
    io.lsu.bvalid := false.B
  }
  /* duplicate single input to multiple outputs */
  io.lsu.rdata := io.out.rdata
  io.lsu.rresp := io.out.rresp
  io.ifu.rdata := io.out.rdata
  io.ifu.rresp := io.out.rresp
}
