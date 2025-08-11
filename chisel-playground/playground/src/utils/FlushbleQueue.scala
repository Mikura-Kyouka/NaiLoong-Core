package core
import chisel3._
import chisel3.util._

class FlushableQueue[T <: Data](gen: T, entries: Int, pipe: Boolean = false, flow: Boolean = false)
    extends Module {

  val io = IO(new Bundle {
    val enq   = Flipped(Decoupled(gen))
    val deq   = Decoupled(gen)
    val flush = Input(Bool()) // 冲刷信号，高电平时瞬间清空
  })

  // === 内部存储结构 ===
  val ram        = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val enq_ptr    = RegInit(0.U(log2Ceil(entries).W))
  val deq_ptr    = RegInit(0.U(log2Ceil(entries).W))
  val maybe_full = RegInit(false.B)

  // === 状态判断 ===
  val ptr_match = enq_ptr === deq_ptr
  val empty     = ptr_match && !maybe_full
  val full      = ptr_match && maybe_full

  // === 默认输出 ===
  io.enq.ready := !full
  io.deq.valid := !empty
  io.deq.bits  := ram(deq_ptr)

  // === 写入逻辑 ===
  when(io.enq.fire) {
    ram(enq_ptr) := io.enq.bits
    enq_ptr := enq_ptr + 1.U
  }

  // === 读取逻辑 ===
  when(io.deq.fire) {
    deq_ptr := deq_ptr + 1.U
  }

  // === 满标志更新 ===
  when(io.enq.fire =/= io.deq.fire) {
    maybe_full := io.enq.fire
  }

  // === Flush 逻辑：瞬间清空 ===
  when(io.flush) {
    enq_ptr    := 0.U
    deq_ptr    := 0.U
    maybe_full := false.B
  }
}
