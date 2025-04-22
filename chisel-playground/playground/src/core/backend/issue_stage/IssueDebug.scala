package core
import chisel3._
import chisel3.util._

// File: src/main/scala/core/FileLoggerBlackBox.scala
//import chisel3.experimental.{HasBlackBoxResource, BlackBox}

class FileLoggerBlackBox extends BlackBox with HasBlackBoxPath {
  val io = IO(new Bundle {
    val clk       = Input(Clock())
    val en        = Input(Bool())
    val pc        = Input(UInt(32.W))
    val queueType = Input(UInt(8.W))
  })
  // 告诉 SBT/Chisel 把上面写的 Verilog 也编译进来
  addPath("playground/src/core/backend/issue_stage/FileLoggerBlackBox.v")
}

// 这里我们把 queueType 当作一个 Scala Int 参数传进来
class IssueDebug(val queueType: Int) extends Module {
  val io = IO(new Bundle {
    val valid     = Input(Bool())
    val ready     = Input(Bool())
    val inst_info = Input(new PipelineConnectIO)
  })

  // 1. 实例化黑盒
  val logger = Module(new FileLoggerBlackBox)
  logger.io.clk       := clock
  // handshake 时才写
  logger.io.en        := io.valid && io.ready
  logger.io.pc        := io.inst_info.pc
  // 把静态参数转成常量信号
  logger.io.queueType := queueType.U(8.W)

  // （可选）如果你还需要在硬件里保留 debugreg ，就继续写：
  val debugreg = Reg(Vec(16, new PipelineConnectIO))
  val write    = RegInit(0.U(log2Ceil(16).W))
  when(io.valid && io.ready) {
    debugreg(write) := io.inst_info
    write := write + 1.U
  }
}
