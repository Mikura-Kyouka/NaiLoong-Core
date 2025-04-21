package core

import chisel3._
import chisel3.util._

class TraceItem extends Bundle {
  val pc    = UInt(32.W)
  val rf_we = Bool()
  val rf_wnum = UInt(5.W)
  val rf_wdata = UInt(32.W)
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
    perItemFifo(i).io.enq.valid := io.in_valids(i)
    perItemFifo(i).io.enq.bits  := io.in_items(i)
  }

  // 把4路 FIFO 输出聚合为一路输出
  val deq_valids = perItemFifo.map(_.io.deq.valid)
  val deq_items  = perItemFifo.map(_.io.deq.bits)

  val chosen_idx = PriorityEncoder(deq_valids)
  val chosen_item = Mux1H(deq_valids.zip(deq_items).map { case (v, d) => (v, d) })

  io.out_valid := deq_valids.reduce(_ || _)
  io.out_item  := chosen_item

  for (i <- 0 until 4) {
    perItemFifo(i).io.deq.ready := io.out_ready && (chosen_idx === i.U)
  }
}

