package Sram
import chisel3._
import chisel3.util._

import Fetch._
import AXI.Signal
import os.stat
import os.read
import os.write

class V_LSU extends BlackBox with HasBlackBoxPath {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val addr = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val wmask = Input(UInt(8.W))
    val r_data = Output(UInt(32.W))
    val wen = Input(Bool())
  })
  addPath("./src/main/resources/lsu-dpic.sv")
}

class SRAM extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new Signal.AXI)
  })

  /* Wrapped as DPI-C */
  // val data_fetch = Module(new Fetch)
  val kernel = Module(new V_LSU)

  val rdata = RegInit(0.U(32.W))

  /*-----DPI-C-----*/
  kernel.io.valid := false.B
  kernel.io.wen := false.B
  kernel.io.addr := io.axi.awaddr
  kernel.io.wdata := io.axi.wdata
  kernel.io.wmask := Cat(0.U(4.W), io.axi.wstrb)
  kernel.io.addr := io.axi.araddr
  rdata := kernel.io.r_data

  // data_fetch.io.pc := io.axi.araddr
  // rdata := data_fetch.io.inst

  io.axi.arready := false.B
  io.axi.rvalid := false.B
  io.axi.awready := false.B
  io.axi.wready := false.B
  io.axi.bvalid := false.B
  val s_wait_valid :: sr_prepare_data :: sr_wait_ready :: sw_prepare_data :: sw_wait_ready :: Nil =
    Enum(5)
  val state = RegInit(s_wait_valid)
  state := MuxLookup(state, s_wait_valid)(
    List(
      s_wait_valid -> Mux(
        io.axi.arvalid | io.axi.awvalid,
        Mux(io.axi.arvalid, sr_prepare_data, sw_prepare_data),
        s_wait_valid
      ),
      /*-----write-----*/
      sw_prepare_data -> Mux(
        ((io.axi.awvalid & io.axi.awready) & (io.axi.wvalid & io.axi.wready)),
        sw_wait_ready,
        sw_prepare_data
      ),
      sw_wait_ready -> Mux(
        (io.axi.bvalid & io.axi.bready),
        s_wait_valid,
        sw_wait_ready
      ),
      /*-----read-----*/
      sr_prepare_data -> Mux(
        (io.axi.arvalid & io.axi.arready),
        sr_wait_ready,
        sr_prepare_data
      ),
      sr_wait_ready -> Mux(
        (io.axi.rready & io.axi.rvalid),
        s_wait_valid,
        sr_wait_ready
      )
    )
  )
  when(state === s_wait_valid) {
    kernel.io.valid := false.B
    kernel.io.wen := false.B
    //
  }.elsewhen(state === sw_prepare_data) {
    /*-----DPI-C-----*/
    kernel.io.valid := false.B
    kernel.io.wen := true.B

    // kernel.io.addr := io.axi.awaddr
    // kernel.io.wdata := io.axi.wdata
    // kernel.io.wmask := Cat(0.U(4.W), io.axi.wstrb)

    io.axi.awready := true.B
    io.axi.wready := true.B
    io.axi.bvalid := false.B
  }.elsewhen(state === sw_wait_ready) {
    kernel.io.valid := true.B
    kernel.io.wen := true.B

    kernel.io.addr := io.axi.awaddr
    kernel.io.wdata := io.axi.wdata
    kernel.io.wmask := Cat(0.U(4.W), io.axi.wstrb)

    io.axi.awready := false.B
    io.axi.wready := false.B
    io.axi.bvalid := true.B
  }.elsewhen(state === sr_prepare_data) {
    kernel.io.valid := true.B
    kernel.io.wen := false.B

    kernel.io.addr := io.axi.araddr

    io.axi.arready := true.B
    io.axi.rvalid := false.B
  }.elsewhen(state === sr_wait_ready) {
    kernel.io.valid := true.B
    kernel.io.wen := false.B

    kernel.io.addr := io.axi.araddr

    io.axi.arready := false.B
    io.axi.rvalid := true.B
  }
  /*
  VALID mustnot be dependent of READY

  Read transaction dependencies
  - the master must not wait for slave to assert ARREADY before asserting ARVALID
  - the slave can wait for ARVALID to be asserted before it asserts ARREADY
  - the slave can assert ARREADY before ARVALID is asserted
  - the slave must wait for both ARVALID and ARREDAY to be asserted before it asserts AVALID to  indicat that valid data is avaible
  - the slave must not wait the master to assert READY before asserting AVALID
  - the master can wait for RVALID to be asserted before it asserts RREADY
  - the master can assert RREADY before RVALID is asserted

  idle -> AWVALID  high

  Write transaction dependencies
  - the master must
   */

  io.axi.rdata := rdata
}
