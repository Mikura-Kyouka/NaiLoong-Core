package core
import chisel3._
import chisel3.util._
// import MmuConfig._

class csr_crmd_bundle extends Bundle{
    val zero = UInt(23.W)
    val datm = UInt(2.W)            // 直接地址翻译模式时，load和store操作的存储访问类型。
    val datf = UInt(2.W)            // 直接地址翻译模式时，取指操作的存储访问类型。
    val pg   = UInt(1.W)            // pg：指示是否开启分页模式 (Paging)。如果 pg = true，则需要进行 TLB 翻译。
    val da   = UInt(1.W)            // da：指示是否为直址 (Direct Address) 模式。如果 da = true，则不走分页，虚拟地址直接用做物理地址。
    val ie   = UInt(1.W)
    val plv  = UInt(2.W)            // plv：当前的特权级 (Privilege Level)。LoongArch 下有 PLV0 ~ PLV3，不同权限会影响 TLB 中的数据访问许可。
}

class csr_prmd_bundle extends Bundle{
  val zero=UInt(29.W)
  val pie =UInt(1.W)
  val pplv=UInt(2.W)
}

class csr_ecfg_bundle extends Bundle{
  val zero31_13=UInt(19.W)
  val lie12_11 =UInt(2.W)
  val zero10   =UInt(1.W)
  val lie9_0   =UInt(10.W)
}

class csr_estat_bundle extends Bundle{
  val zero31   =UInt(1.W)
  val esubcode =UInt(9.W)
  val ecode    =UInt(6.W)
  val zero15_13=UInt(3.W)
  val is12     =UInt(1.W)
  val is11     =UInt(1.W)
  val zero10   =UInt(1.W)
  val is9_2    =UInt(8.W)
  val is1_0    =UInt(2.W)
}

class csr_eentry_bundle extends Bundle {
  val va  =UInt(26.W)
  val zero=UInt(6.W)
}

class csr_tlbidx_bundle extends Bundle {
    val ne = UInt(1.W)              // 该位为1表示该TLB表项为空（无效TLB表项），为0表示该TLB表项非空（有效TLB表项）。
    val zero30 = UInt(1.W)
    val ps = UInt(6.W)              // 执行TLBRD指令时，所读取TLB表项的PS域的值记录到这里。执行TLBWR和TLBFILL指令，写入的TLB表项的PS域的值来自于此。
    val zero23_16 = UInt(8.W)
    val idx = UInt(16.W)            // 执行TLBRD和TLBWR指令时，访问TLB表项的索引值来自于此。
}

class csr_tlbehi_bundle extends Bundle {
  val vppn    =UInt(19.W)
  val zero12_0=UInt(13.W)
}

class csr_tlbelo_bundle extends Bundle{
    val ppn   = UInt(24.W)          // 页表的物理页号（PPN）
    val zero7 = UInt(1.W)
    val g     = UInt(1.W)           // 页表项的全局标志位（G）。
    val mat   = UInt(2.W)           // 页表项的存储访问类型（MAT）。
    val plv   = UInt(2.W)           // 页表项的特权等级（PLV）。
    val d     = UInt(1.W)           // 页表项的脏位（D）。
    val v     = UInt(1.W)           // 页表项的有效位（V）。
}

class csr_asid_bundle extends Bundle{
    val zero31_24 = UInt(8.W)
    val asidbits  = UInt(8.W)       // ASID域的位宽。其直接等于这个域的数值。
    val zero15_10 = UInt(6.W)
    val asid      = UInt(10.W)      // 当前执行的程序所对应的地址空间标识符。
}

class csr_pgdx_bundle extends Bundle{
  //NOTE:pgd_x -> pgdl or pgdh or pgd
  val base    =UInt(20.W)
  val zero11_0=UInt(12.W)
}

class csr_tcfg_bundle extends Bundle{
  val initval =UInt(30.W)
  val periodic=UInt(1.W)
  val en      =UInt(1.W)
}

class csr_llbctl_bundle extends Bundle{
  val zero31_3=UInt(29.W)
  val klo     =UInt(1.W)
  val wcllb   =UInt(1.W)
  val rollb   =UInt(1.W)
}

class csr_tlbrentry_bundle extends Bundle{
  val pa      =UInt(26.W)
  val zero5_0 =UInt(6.W)
}

class csr_dmw_bundle extends Bundle {
    val vseg     = UInt(3.W)         // 直接映射窗口的虚地址的[31:29]位。
    val zero28   = UInt(1.W)
    val pseg     = UInt(3.W)         // 直接映射窗口的物理地址的[31:29]位。
    val zero24_6 = UInt(19.W)
    val mat      = UInt(2.W)         // 虚地址落在该映射窗口下访存操作的存储访问类型。
    val plv3     = UInt(1.W)         // 为1表示在特权等级PLV3下可以使用该窗口的配置进行直接映射地址翻译。
    val zero2_1  = UInt(2.W)
    val plv0     = UInt(1.W)         // 为1表示在特权等级PLV0下可以使用该窗口的配置进行直接映射地址翻译。
}

class csr_read_bundle extends Bundle {
  val csr_num = Input(UInt(14.W)) // csr寄存器的id
  val csr_data = Output(UInt(32.W)) // csr寄存器的值  
}

class csr_write_bundle extends Bundle {
  val csr_num = Input(UInt(14.W)) // csr寄存器的id
  val csr_data = Input(UInt(32.W)) // csr寄存器的值  
}

class csr_excp_bundle extends Bundle {
  val valid = Input(Bool()) // 异常有效
  val exceptionPC = Input(UInt(32.W)) // 异常发生时的pc
  val exceptionVec = Input(UInt(16.W)) // 异常向量
  val exceptionNewPC = Output(UInt(32.W)) // 异常处理完成后返回的pc
}