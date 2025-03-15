package core

import chisel3._
import chisel3.util._

class PhysRegAddrIO extends Bundle {
  val addr = UInt(7.W)
  val valid = Bool()
}

class ExecResult extends Bundle {
  val data = UInt(32.W)
  val rdPhys = new PhysRegAddrIO // 目标物理寄存器
  val fuType = UInt(2.W)
}

class StageResult extends Bundle {
  val exec = new ExecResult
  val valid = Bool()
}

// 旁路控制和数据选择单元
class BypassNetwork extends Module {
  val io = IO(new Bundle {
    // 四个FU的执行结果输入
    val alu0Result = Input(new StageResult)
    val alu1Result = Input(new StageResult)
    val mduResult = Input(new StageResult)
    val lsuResult = Input(new StageResult)
    
    // 四个FU的旁路数据输出
    val alu0Bypass = Output(Vec(2, new ExecResult)) // 2个源寄存器的旁路数据
    val alu1Bypass = Output(Vec(2, new ExecResult))
    val mduBypass = Output(Vec(2, new ExecResult))
    val lsuBypass = Output(Vec(2, new ExecResult))
    
    // 读取的源物理寄存器地址输入
    val alu0SrcRegs = Input(Vec(2, new PhysRegAddrIO))
    val alu1SrcRegs = Input(Vec(2, new PhysRegAddrIO))
    val mduSrcRegs = Input(Vec(2, new PhysRegAddrIO))
    val lsuSrcRegs = Input(Vec(2, new PhysRegAddrIO))
    
    // 写回阶段的旁路
    val wbResults = Input(Vec(4, new StageResult)) // 最多4个写回结果
    
    // 旁路选择信号输出
    val alu0BypassSelect = Output(Vec(2, UInt(3.W)))  // 3位: 000-无旁路, 001-ALU0, 010-ALU1, 011-MDU, 100-LSU, 101-WB0...
    val alu1BypassSelect = Output(Vec(2, UInt(3.W)))
    val mduBypassSelect = Output(Vec(2, UInt(3.W)))
    val lsuBypassSelect = Output(Vec(2, UInt(3.W)))
  })
  
  // 旁路逻辑函数 - 比较源寄存器与结果寄存器是否匹配
  def bypassMatch(srcReg: PhysRegAddrIO, resultReg: PhysRegAddrIO, resultValid: Bool): Bool = {
    (srcReg.valid && resultReg.valid && resultValid && srcReg.addr === resultReg.addr)
  }
  
  // 为每个功能单元的每个源寄存器实现旁路选择逻辑
  // ALU0旁路选择
  for (i <- 0 until 2) {
    val bypassSel = WireDefault(0.U(3.W))
    
    when (bypassMatch(io.alu0SrcRegs(i), io.alu0Result.exec.rdPhys, io.alu0Result.valid)) {
      bypassSel := 1.U
    }.elsewhen (bypassMatch(io.alu0SrcRegs(i), io.alu1Result.exec.rdPhys, io.alu1Result.valid)) {
      bypassSel := 2.U
    }.elsewhen (bypassMatch(io.alu0SrcRegs(i), io.mduResult.exec.rdPhys, io.mduResult.valid)) {
      bypassSel := 3.U
    }.elsewhen (bypassMatch(io.alu0SrcRegs(i), io.lsuResult.exec.rdPhys, io.lsuResult.valid)) {
      bypassSel := 4.U
    }.otherwise {
      // 写回阶段
      for (j <- 0 until 4) {
        when (bypassMatch(io.alu0SrcRegs(i), io.wbResults(j).exec.rdPhys, io.wbResults(j).valid)) {
          bypassSel := (5 + j).U  // 写回阶段旁路
        }
      }
    }
    io.alu0BypassSelect(i) := bypassSel
  }
  
  // ALU1旁路选择
  for (i <- 0 until 2) {
    val bypassSel = WireDefault(0.U(3.W))
    
    when (bypassMatch(io.alu1SrcRegs(i), io.alu0Result.exec.rdPhys, io.alu0Result.valid)) {
      bypassSel := 1.U
    }.elsewhen (bypassMatch(io.alu1SrcRegs(i), io.alu1Result.exec.rdPhys, io.alu1Result.valid)) {
      bypassSel := 2.U
    }.elsewhen (bypassMatch(io.alu1SrcRegs(i), io.mduResult.exec.rdPhys, io.mduResult.valid)) {
      bypassSel := 3.U
    }.elsewhen (bypassMatch(io.alu1SrcRegs(i), io.lsuResult.exec.rdPhys, io.lsuResult.valid)) {
      bypassSel := 4.U
    }.otherwise {
      for (j <- 0 until 4) {
        when (bypassMatch(io.alu1SrcRegs(i), io.wbResults(j).exec.rdPhys, io.wbResults(j).valid)) {
          bypassSel := (5 + j).U
        }
      }
    }
    io.alu1BypassSelect(i) := bypassSel
  }
  
  // MDU旁路选择
  for (i <- 0 until 2) {
    val bypassSel = WireDefault(0.U(3.W))
    
    when (bypassMatch(io.mduSrcRegs(i), io.alu0Result.exec.rdPhys, io.alu0Result.valid)) {
      bypassSel := 1.U
    }.elsewhen (bypassMatch(io.mduSrcRegs(i), io.alu1Result.exec.rdPhys, io.alu1Result.valid)) {
      bypassSel := 2.U
    }.elsewhen (bypassMatch(io.mduSrcRegs(i), io.mduResult.exec.rdPhys, io.mduResult.valid)) {
      bypassSel := 3.U
    }.elsewhen (bypassMatch(io.mduSrcRegs(i), io.lsuResult.exec.rdPhys, io.lsuResult.valid)) {
      bypassSel := 4.U
    }.otherwise {
      for (j <- 0 until 4) {
        when (bypassMatch(io.mduSrcRegs(i), io.wbResults(j).exec.rdPhys, io.wbResults(j).valid)) {
          bypassSel := (5 + j).U
        }
      }
    }
    io.mduBypassSelect(i) := bypassSel
  }
  
  // LSU旁路选择
  for (i <- 0 until 2) {
    val bypassSel = WireDefault(0.U(3.W))
    
    when (bypassMatch(io.lsuSrcRegs(i), io.alu0Result.exec.rdPhys, io.alu0Result.valid)) {
      bypassSel := 1.U
    }.elsewhen (bypassMatch(io.lsuSrcRegs(i), io.alu1Result.exec.rdPhys, io.alu1Result.valid)) {
      bypassSel := 2.U
    }.elsewhen (bypassMatch(io.lsuSrcRegs(i), io.mduResult.exec.rdPhys, io.mduResult.valid)) {
      bypassSel := 3.U
    }.elsewhen (bypassMatch(io.lsuSrcRegs(i), io.lsuResult.exec.rdPhys, io.lsuResult.valid)) {
      bypassSel := 4.U
    }.otherwise {
      for (j <- 0 until 4) {
        when (bypassMatch(io.lsuSrcRegs(i), io.wbResults(j).exec.rdPhys, io.wbResults(j).valid)) {
          bypassSel := (5 + j).U
        }
      }
    }
    io.lsuBypassSelect(i) := bypassSel
  }
  
  // 旁路数据选择器 - 连接到各个源寄存器
  // ALU0 旁路数据
  for (i <- 0 until 2) {
    val bypassMux = MuxCase(
      Wire(new ExecResult),
      Seq(
        (io.alu0BypassSelect(i) === 1.U) -> io.alu0Result.exec,
        (io.alu0BypassSelect(i) === 2.U) -> io.alu1Result.exec,
        (io.alu0BypassSelect(i) === 3.U) -> io.mduResult.exec,
        (io.alu0BypassSelect(i) === 4.U) -> io.lsuResult.exec,
        (io.alu0BypassSelect(i) === 5.U) -> io.wbResults(0).exec,
        (io.alu0BypassSelect(i) === 6.U) -> io.wbResults(1).exec,
        (io.alu0BypassSelect(i) === 7.U) -> io.wbResults(2).exec,
        (io.alu0BypassSelect(i) === 8.U) -> io.wbResults(3).exec
      )
    )
    io.alu0Bypass(i) := bypassMux
  }
  
  // ALU1 旁路数据
  for (i <- 0 until 2) {
    val bypassMux = MuxCase(
      Wire(new ExecResult),
      Seq(
        (io.alu1BypassSelect(i) === 1.U) -> io.alu0Result.exec,
        (io.alu1BypassSelect(i) === 2.U) -> io.alu1Result.exec,
        (io.alu1BypassSelect(i) === 3.U) -> io.mduResult.exec,
        (io.alu1BypassSelect(i) === 4.U) -> io.lsuResult.exec,
        (io.alu1BypassSelect(i) === 5.U) -> io.wbResults(0).exec,
        (io.alu1BypassSelect(i) === 6.U) -> io.wbResults(1).exec,
        (io.alu1BypassSelect(i) === 7.U) -> io.wbResults(2).exec,
        (io.alu1BypassSelect(i) === 8.U) -> io.wbResults(3).exec
      )
    )
    io.alu1Bypass(i) := bypassMux
  }
  
  // MDU 旁路数据
  for (i <- 0 until 2) {
    val bypassMux = MuxCase(
      Wire(new ExecResult),
      Seq(
        (io.mduBypassSelect(i) === 1.U) -> io.alu0Result.exec,
        (io.mduBypassSelect(i) === 2.U) -> io.alu1Result.exec,
        (io.mduBypassSelect(i) === 3.U) -> io.mduResult.exec,
        (io.mduBypassSelect(i) === 4.U) -> io.lsuResult.exec,
        (io.mduBypassSelect(i) === 5.U) -> io.wbResults(0).exec,
        (io.mduBypassSelect(i) === 6.U) -> io.wbResults(1).exec,
        (io.mduBypassSelect(i) === 7.U) -> io.wbResults(2).exec,
        (io.mduBypassSelect(i) === 8.U) -> io.wbResults(3).exec
      )
    )
    io.mduBypass(i) := bypassMux
  }
  
  // LSU 旁路数据
  for (i <- 0 until 2) {
    val bypassMux = MuxCase(
      Wire(new ExecResult),
      Seq(
        (io.lsuBypassSelect(i) === 1.U) -> io.alu0Result.exec,
        (io.lsuBypassSelect(i) === 2.U) -> io.alu1Result.exec,
        (io.lsuBypassSelect(i) === 3.U) -> io.mduResult.exec,
        (io.lsuBypassSelect(i) === 4.U) -> io.lsuResult.exec,
        (io.lsuBypassSelect(i) === 5.U) -> io.wbResults(0).exec,
        (io.lsuBypassSelect(i) === 6.U) -> io.wbResults(1).exec,
        (io.lsuBypassSelect(i) === 7.U) -> io.wbResults(2).exec,
        (io.lsuBypassSelect(i) === 8.U) -> io.wbResults(3).exec
      )
    )
    io.lsuBypass(i) := bypassMux
  }
}
