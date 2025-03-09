package core

import chisel3._
import chisel3.util._

object BypassParams {
  val ISSUE_WIDTH = 4
  val PHY_REG_BITS = 7
  val DATA_WIDTH = 32
}

class BypassBundle extends Bundle {
  val valid = Bool()
  val rd = UInt(BypassParams.PHY_REG_BITS.W)
  val data = UInt(BypassParams.DATA_WIDTH.W)
  val isLoad = Bool()
}

class BypassNetwork extends Module {
  val io = IO(new Bundle {
    // 执行阶段输入
    val eStage = Input(Vec(BypassParams.ISSUE_WIDTH, new BypassBundle))
    // 写回阶段输入
    val wStage = Input(Vec(BypassParams.ISSUE_WIDTH, new BypassBundle))
    // 操作数请求
    val req = Input(Vec(BypassParams.ISSUE_WIDTH, new Bundle {
      val rj = UInt(BypassParams.PHY_REG_BITS.W)
      val rk = UInt(BypassParams.PHY_REG_BITS.W)
      val isUse = Vec(2, Bool())
    }))
    // 旁路结果输出
    val out = Output(Vec(BypassParams.ISSUE_WIDTH, new Bundle {
      val dataRJ = UInt(BypassParams.DATA_WIDTH.W)
      val dataRK = UInt(BypassParams.DATA_WIDTH.W)
    }))
  })

  for (i <- 0 until BypassParams.ISSUE_WIDTH) {
    // 生成两个选择器（rj和rk各一个）
    def createSelector(regNum: UInt): UInt = {
      val sources = Seq.tabulate(2 * BypassParams.ISSUE_WIDTH) { n =>
        if (n < BypassParams.ISSUE_WIDTH) { // W阶段
          val j = BypassParams.ISSUE_WIDTH - 1 - n
          val valid = io.wStage(j).valid
          val matchReg = io.wStage(j).rd === regNum
          val loadCheck = !io.wStage(j).isLoad || (j.U === i.U)
          val cond = valid && matchReg && loadCheck
          (cond, io.wStage(j).data)
        } else { // E阶段
          val j = BypassParams.ISSUE_WIDTH - 1 - (n - BypassParams.ISSUE_WIDTH)
          val eValid = RegNext(io.eStage(j).valid)
          val eRd = RegNext(io.eStage(j).rd)
          val eData = RegNext(io.eStage(j).data)
          val eIsLoad = RegNext(io.eStage(j).isLoad)
          val cond = eValid && (eRd === regNum) && !eIsLoad
          (cond, eData)
        }
      }

      sources.foldLeft(0.U(BypassParams.DATA_WIDTH.W)) { 
        case (prev, (cond, data)) => Mux(cond, data, prev)
      }
    }

    io.out(i).dataRJ := Mux(io.req(i).isUse(0), createSelector(io.req(i).rj), 0.U)
    io.out(i).dataRK := Mux(io.req(i).isUse(1), createSelector(io.req(i).rk), 0.U)
  }
}


