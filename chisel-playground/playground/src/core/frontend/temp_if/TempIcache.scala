import chisel3._

// 直接映射cache
object CacheConfig {
  import chisel3.util._
  def LINE_WIDTH = 128
  def LINE_WORD_NUM = (LINE_WIDTH / 32)
  def LINE_BIT_NUM = log2Ceil(LINE_WIDTH)

  def LINE_NUM = 16
  def INDEX_BIT_NUM = log2Ceil(LINE_NUM)

  def TAG_BIT_NUM = 32 - INDEX_BIT_NUM - LINE_BIT_NUM
}

class CacheLine extends Bundle {
  import CacheConfig._
  val tag = UInt(TAG_BIT_NUM.W)      // 21-bit tag
  val data = Vec(CacheConfig.LINE_WORD_NUM, UInt(32.W)) // 128-bit data
  val valid = Bool()        // 1-bit valid flag
}

class TempIcache extends Module {
  import chisel3.util._
  import CacheConfig._
  val io = IO(new Bundle {
    val waddr = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val wen = Input(Bool())
  })

  val cache = Reg(Vec(CacheConfig.LINE_NUM, new CacheLine))
  dontTouch(cache)
  when(io.wen) {
    val index = io.waddr(INDEX_BIT_NUM + LINE_BIT_NUM - 1, LINE_BIT_NUM)
    val tag = io.waddr(31, INDEX_BIT_NUM + LINE_BIT_NUM)
    val offset = io.waddr(3, 2)
    cache(index).valid := true.B
    cache(index).tag := tag
    cache(index).data(offset) := io.wdata
  }
}