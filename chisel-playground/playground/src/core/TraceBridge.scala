package core

import chisel3._
import chisel3.util._

class TraceItem extends Bundle {
  val pc    = UInt(32.W)
  val rf_we = UInt(4.W)
  val rf_wnum = UInt(5.W)
  val rf_wdata = UInt(32.W)
  val valid = Bool()
}

class TraceBridgeIO extends Bundle {
  val in_valids  = Input(Vec(4, Bool()))
  val in_items   = Input(Vec(4, new TraceItem))
  
  // 输出给trace比对模块
  val out_valid  = Output(Bool())
  val out_item   = Output(new TraceItem)
  val out_ready  = Input(Bool())
}

class TraceBridge extends Module {
  val io = IO(new TraceBridgeIO)
  
  // 每个写回一路 FIFO
  val perItemFifo = Seq.fill(4)(Module(new Queue(new TraceItem, 8)))
  for (i <- 0 until 4) {
    perItemFifo(i).io.enq.bits := io.in_items(i)
    perItemFifo(i).io.enq.bits.valid := io.in_valids(i)
  }
  
  when (io.in_items(0).valid || io.in_items(1).valid || io.in_items(2).valid || io.in_items(3).valid) {
    for (i <- 0 until 4) {
      perItemFifo(i).io.enq.valid := io.in_valids(i)
    }
  } .otherwise {
    for (i <- 0 until 4) {
      perItemFifo(i).io.enq.valid := false.B
    }
  }

  val roundRobinCounter = RegInit(0.U(3.W))
  
  val validVec = VecInit(perItemFifo.map(_.io.deq.bits.valid))
  val deqValidVec = VecInit(perItemFifo.map(_.io.deq.valid))
  val itemVec = VecInit(perItemFifo.map(_.io.deq.bits))
  
  val current_valid = validVec(roundRobinCounter)
  val current_deq_valid = deqValidVec(roundRobinCounter)
  val current_item = itemVec(roundRobinCounter)
  
  io.out_valid := current_valid
  io.out_item := current_item
  
  for (i <- 0 until 4) {
    perItemFifo(i).io.deq.ready := io.out_ready && (roundRobinCounter === i.U) && current_deq_valid
  }
  
  when (!deqValidVec.reduce(_ || _)) {
    roundRobinCounter := 0.U
  }.elsewhen (io.out_ready && current_deq_valid) {
    roundRobinCounter := Mux(roundRobinCounter === 3.U, 0.U, roundRobinCounter + 1.U)
  }
}
