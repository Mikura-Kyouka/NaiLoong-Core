package core

import chisel3._
import chisel3.util._

class SignedDivider extends Module {
  val io = IO(new Bundle {
    val aclk = Input(Clock())

    val s_axis_divisor_tdata  = Input(SInt(32.W))
    val s_axis_divisor_tready = Output(Bool())
    val s_axis_divisor_tvalid = Input(Bool())

    val s_axis_dividend_tdata  = Input(SInt(32.W))
    val s_axis_dividend_tready = Output(Bool())
    val s_axis_dividend_tvalid = Input(Bool())

    val m_axis_dout_tdata  = Output(UInt(64.W))
    val m_axis_dout_tready = Input(Bool())
    val m_axis_dout_tvalid = Output(Bool())
  })

  val idle :: calc :: waitGap :: Nil = Enum(3)
  val state = RegInit(idle)

  val dividend = Reg(SInt(32.W))
  val divisor  = Reg(SInt(32.W))

  val quotient = Reg(SInt(32.W))
  val remainder = Reg(SInt(32.W))

  val latencyCounter = RegInit(0.U(6.W))
  val gapCounter = RegInit(0.U(4.W))

  val inputReady = (state === idle)

  io.s_axis_dividend_tready := inputReady
  io.s_axis_divisor_tready := inputReady

  val inputFire = io.s_axis_dividend_tvalid && io.s_axis_divisor_tvalid && inputReady

  val outputValid = RegInit(false.B)

  io.m_axis_dout_tvalid := outputValid
  io.m_axis_dout_tdata := Cat(quotient.asUInt, remainder.asUInt)

  switch(state) {
    is(idle) {
      outputValid := false.B
      when(inputFire) {
        dividend := io.s_axis_dividend_tdata
        divisor := io.s_axis_divisor_tdata
        latencyCounter := 37.U
        state := calc
      }
    }

    is(calc) {
      // 模拟除法 latency：第37周期完成计算
      when(latencyCounter === 1.U) {
        // 执行有符号除法
        when(divisor === 0.S) {
          quotient := -1.S
          remainder := dividend
        }.otherwise {
          quotient := dividend / divisor
          remainder := dividend - divisor * quotient
        }
        outputValid := true.B
        gapCounter := 8.U
        state := waitGap
      }.otherwise {
        latencyCounter := latencyCounter - 1.U
      }
    }

    is(waitGap) {
      // 等待输出握手完成 + 至少间隔 8 周期
      when(outputValid && io.m_axis_dout_tready) {
        outputValid := false.B
      }
      when(gapCounter === 0.U && !outputValid) {
        state := idle
      }.otherwise {
        when(gapCounter =/= 0.U) {
          gapCounter := gapCounter - 1.U
        }
      }
    }
  }
}
