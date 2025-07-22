package core

import chisel3._
import chisel3.util._

class TraceItem extends Bundle {
  val pc    = UInt(32.W)
  val rf_we = UInt(4.W)
  val rf_wnum = UInt(5.W)
  val rf_wdata = UInt(32.W)
  val valid = Bool()
  val seq_num = UInt(32.W)
}

class TraceBridgeIO extends Bundle {
  val in_valids  = Input(Vec(RobConfig.ROB_CMT_NUM, Bool()))
  val in_items   = Input(Vec(RobConfig.ROB_CMT_NUM, new TraceItem))
  
  // 输出给trace比对模块
  val out_valid  = Output(Bool())
  val out_item   = Output(new TraceItem)
  val out_ready  = Input(Bool())
}

class TraceBridge extends Module {
  val io = IO(new TraceBridgeIO)
  
  val globalSeqCounter = RegInit(0.U(32.W))

  // 每个写回一路 FIFO
  val perItemFifo = Seq.fill(RobConfig.ROB_CMT_NUM)(Module(new Queue(new TraceItem, 8)))
  for (i <- 0 until RobConfig.ROB_CMT_NUM) {
    perItemFifo(i).io.enq.bits := io.in_items(i)
    perItemFifo(i).io.enq.bits.valid := io.in_valids(i)
    perItemFifo(i).io.enq.bits.seq_num := globalSeqCounter
  }
  
  when (io.in_items.map(_.valid).reduce(_ || _)) {
    for (i <- 0 until RobConfig.ROB_CMT_NUM) {
      perItemFifo(i).io.enq.valid := io.in_valids(i)
      globalSeqCounter := globalSeqCounter + 1.U
    }
  }.otherwise {
    for (i <- 0 until RobConfig.ROB_CMT_NUM) {
      perItemFifo(i).io.enq.valid := false.B
    }
  }

  val roundRobinCounter = WireInit(0.U(3.W))

  // Initialize with ready signals from consumer
  for (i <- 0 until RobConfig.ROB_CMT_NUM) {
    perItemFifo(i).io.deq.ready := false.B
  }

  // Find the smallest sequence number among valid items
  val validDequeues = perItemFifo.map(fifo => fifo.io.deq.valid)
  val seqNumbers = perItemFifo.map(fifo => fifo.io.deq.bits.seq_num)

  // Default to first FIFO
  val smallestIdxWire = Wire(UInt(2.W))
  smallestIdxWire := 0.U

  // Compare sequence numbers to find the smallest
  when(validDequeues(1) && (!validDequeues(0) || seqNumbers(1) < seqNumbers(0))) {
    smallestIdxWire := 1.U
  }
  // when(validDequeues(2) && (!validDequeues(0) || seqNumbers(2) < seqNumbers(0)) && 
  //     (!validDequeues(1) || seqNumbers(2) < seqNumbers(1))) {
  //   smallestIdxWire := 2.U
  // }
  // when(validDequeues(3) && (!validDequeues(0) || seqNumbers(3) < seqNumbers(0)) && 
  //     (!validDequeues(1) || seqNumbers(3) < seqNumbers(1)) && 
  //     (!validDequeues(2) || seqNumbers(3) < seqNumbers(2))) {
  //   smallestIdxWire := 3.U
  // }

  // Update roundRobinCounter to select the smallest sequence number
  roundRobinCounter := smallestIdxWire
  
  val validVec = VecInit(perItemFifo.map(_.io.deq.bits.valid))
  val deqValidVec = VecInit(perItemFifo.map(_.io.deq.valid))
  val itemVec = VecInit(perItemFifo.map(_.io.deq.bits))
  
  val current_valid = validVec(roundRobinCounter)
  val current_deq_valid = deqValidVec(roundRobinCounter)
  val current_item = itemVec(roundRobinCounter)
  
  io.out_valid := current_valid

  val validatedOutItem = Wire(new TraceItem)
  validatedOutItem.pc := current_item.pc
  validatedOutItem.rf_we := Mux(current_deq_valid, current_item.rf_we, 0.U)
  validatedOutItem.rf_wnum := current_item.rf_wnum
  validatedOutItem.rf_wdata := current_item.rf_wdata
  validatedOutItem.valid := current_valid
  validatedOutItem.seq_num := current_item.seq_num

  io.out_item := Mux(current_deq_valid, validatedOutItem, 0.U.asTypeOf(validatedOutItem))
  
  for (i <- 0 until RobConfig.ROB_CMT_NUM) {
    perItemFifo(i).io.deq.ready := io.out_ready && (roundRobinCounter === i.U) && current_deq_valid
  }
  
  // when (!deqValidVec.reduce(_ || _)) {
  //   roundRobinCounter := 0.U
  // }.elsewhen (io.out_ready) {
  //   roundRobinCounter := Mux(roundRobinCounter === 3.U, 0.U, roundRobinCounter + 1.U)
  // }
}
