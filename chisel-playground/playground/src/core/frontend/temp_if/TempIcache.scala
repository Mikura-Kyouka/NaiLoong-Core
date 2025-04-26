package core
import chisel3._
import chisel3.util._

import CacheConfig._
import core.LSUOpType.sw

// 直接映射cache
object CacheConfig {
  def LINE_WIDTH = 128
  def LINE_WORD_NUM = (LINE_WIDTH / 32)
  def LINE_BIT_NUM = log2Ceil(LINE_WIDTH)
  def OFFSET_BIT_NUM = log2Ceil(LINE_WIDTH / 8)

  def LINE_NUM = 16
  def INDEX_BIT_NUM = log2Ceil(LINE_NUM)

  def TAG_BIT_NUM = 32 - INDEX_BIT_NUM - OFFSET_BIT_NUM
}

class CacheLine extends Bundle {
  val tag = UInt(TAG_BIT_NUM.W)      // 21-bit tag
  val data = Vec(CacheConfig.LINE_WORD_NUM, UInt(32.W)) // 128-bit data
  val valid = Vec(CacheConfig.LINE_WORD_NUM, Bool())
}

class TempIcache extends Module {
  val io = IO(new Bundle {
    val waddr = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val wen = Input(Bool())

    val valid = Output(Bool())
    val ready = Input(Bool())
    val inst0 = Output(new inst_info_)
    val inst1 = Output(new inst_info_)
    val inst2 = Output(new inst_info_)
    val inst3 = Output(new inst_info_)

    val new_pc = Input(UInt(32.W))
    val flush = Input(Bool())

    val read_pc = Output(UInt(32.W))
  })

  val cache = RegInit(VecInit(Seq.fill(CacheConfig.LINE_NUM)(0.U.asTypeOf(new CacheLine))))
  dontTouch(cache)
  when(io.wen) {
    val index = io.waddr(INDEX_BIT_NUM + OFFSET_BIT_NUM - 1, OFFSET_BIT_NUM)
    val tag = io.waddr(31, INDEX_BIT_NUM + OFFSET_BIT_NUM)
    val offset = io.waddr(3, 2)
    cache(index).valid(offset) := true.B
    when(offset === 3.U) {
      cache(index).tag := tag
    }
    cache(index).data(offset) := io.wdata
  }

  // read
  val read_pc = RegInit("h1c000000".U(32.W))
  val raddr0 = Cat(read_pc(31, 4), 0.U(4.W))
  val raddr1 = Cat(read_pc(31, 4), 4.U(4.W))
  val raddr2 = Cat(read_pc(31, 4), 8.U(4.W))
  val raddr3 = Cat(read_pc(31, 4), 12.U(4.W))

  val index = read_pc(INDEX_BIT_NUM + OFFSET_BIT_NUM - 1, OFFSET_BIT_NUM)
  dontTouch(index)

  val tag0 = raddr0(31, INDEX_BIT_NUM + OFFSET_BIT_NUM)
  val tag1 = raddr1(31, INDEX_BIT_NUM + OFFSET_BIT_NUM)
  val tag2 = raddr2(31, INDEX_BIT_NUM + OFFSET_BIT_NUM)
  val tag3 = raddr3(31, INDEX_BIT_NUM + OFFSET_BIT_NUM)

  val hit0 = cache(index).valid(0) && cache(index).tag === tag0
  val hit1 = cache(index).valid(1) && cache(index).tag === tag1
  val hit2 = cache(index).valid(2) && cache(index).tag === tag2
  val hit3 = cache(index).valid(3) && cache(index).tag === tag3
  dontTouch(hit0)
  dontTouch(hit1)
  dontTouch(hit2)
  dontTouch(hit3)

  val cnt = RegInit(0.U(3.W))
  when(io.valid && io.ready) {
    cnt := 0.U
  }.elsewhen(cnt < 3.U) {
    cnt := cnt + 1.U
  }

  io.valid := (hit0 || raddr0 < read_pc) && 
              (hit1 || raddr1 < read_pc) && 
              (hit2 || raddr2 < read_pc) && 
              (hit3 || raddr3 < read_pc) && cnt === 3.U

  io.inst0.pc := raddr0
  io.inst0.inst := cache(index).data(0)
  io.inst0.valid := raddr0 < read_pc || hit0
  io.inst1.pc := raddr1
  io.inst1.inst := cache(index).data(1)
  io.inst1.valid := raddr1 < read_pc || hit1
  io.inst2.pc := raddr2
  io.inst2.inst := cache(index).data(2)
  io.inst2.valid := raddr2 < read_pc || hit2
  io.inst3.pc := raddr3
  io.inst3.inst := cache(index).data(3)
  io.inst3.valid := raddr3 < read_pc || hit3
  io.read_pc := read_pc

  when(io.flush) {
    read_pc := io.new_pc
  }

  when(io.valid && io.ready) {
    read_pc := raddr0 + 16.U
  }
}