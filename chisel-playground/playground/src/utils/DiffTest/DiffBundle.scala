package core
import chisel3._
import chisel3.util._

class DiffInstrBundle extends Bundle{
  val valid = Bool()                              // 提交有效信号
  val pc = UInt(32.W)                             // 当前提交指令的pc
  val instr = UInt(32.W)                          // 当前提交指令的指令码
  val skip = Bool()                               // 跳过当前指令的比对（没实现）
  val is_TLBFILL = Bool()                         // tlbfill指令使能
  val TLBFILL_index = UInt(log2Ceil(32).W)   // tlbfill指令对应的tlb表项索引
  val is_CNTinst = Bool()                         // 与计时器相关的指令，提交指令为rdcntv{l/h}.w 或 rdcntid 时该位拉高
  val timer_64_value = UInt(64.W)                 // 当前指令读出的64位计数器值(StableCounter)
  val wen = Bool()                                // 提交指令通用寄存器写使能
  val wdest = UInt(8.W)                           // 提交指令写通用寄存器索引
  val wdata = UInt(32.W)                          // 提交指令写通用寄存器数据
  val csr_rstat = Bool()                          // 当提交指令为csrrd、csrwr、csrxchg，同时该指令对应的csr寄存器为estat寄存器时该位拉高
  val csr_data = UInt(32.W)                       // 当csr_rstat == 1时，当前指令读取到的csr寄存器(estat)的值
}

class DiffStoreBundle extends Bundle {
  val valid = UInt(8.W)                          // 提交指令对应的store指令的有效信号
  val paddr = UInt(32.W)                         // 提交指令对应的store指令的物理地址
  val vaddr = UInt(32.W)                         // 提交指令对应的store指令的虚拟地址
  val data = UInt(32.W)                          // 提交指令对应的store指令的数据
}

class DiffExcpBundle extends Bundle {
  val excp_valid = Bool()
  val eret = Bool()
  val intrNo = UInt(11.W)
  val cause = UInt(6.W)
  val exceptionPC = UInt(32.W)
  val exceptionInst = UInt(32.W)
}