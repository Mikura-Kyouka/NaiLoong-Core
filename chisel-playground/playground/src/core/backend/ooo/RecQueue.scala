package core

import chisel3._
import chisel3.util._

class RecQueue[T <: Data](gen: T, entries: Int, pipe: Boolean = false, hasFlush: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
    val flush = if (hasFlush) Some(Input(Bool())) else None
  })

  val mem = Reg(Vec(entries, gen))
  val valid = RegInit(VecInit(Seq.fill(entries)(false.B)))

  val head = RegInit(0.U(log2Ceil(entries).W))
  val tail = RegInit(0.U(log2Ceil(entries).W))
  val count = RegInit(0.U(log2Ceil(entries + 1).W))

  def wrap(ptr: UInt): UInt = Mux(ptr === (entries - 1).U, 0.U, ptr + 1.U)

  io.enq.ready := count =/= entries.U
  when(io.enq.fire) {
    mem(tail) := io.enq.bits
    valid(tail) := true.B
    tail := wrap(tail)
    count := count + 1.U
  }

  io.deq.valid := count =/= 0.U && valid(head)
  io.deq.bits := mem(head)
  when(io.deq.fire) {
    valid(head) := false.B
    head := wrap(head)
    count := count - 1.U
  }

  if (pipe) {
    io.deq.valid := RegNext(io.deq.valid, false.B)
    io.deq.bits := RegNext(io.deq.bits)
  }

  // Flush
  if (hasFlush) {
    when(io.flush.get) {
      valid.foreach(_ := false.B)
      head := 0.U
      tail := 0.U
      count := 0.U
    }
  }
}
