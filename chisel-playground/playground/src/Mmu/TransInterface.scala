package core
import chisel3._
import chisel3.util._
import MmuConfig._

class TransInterface extends Module {
    val io = IO(new Bundle {
        val vaddr = Input(UInt(ADDR_WIDTH.W))
        val paddr = Output(UInt(ADDR_WIDTH.W))
        val crmd = Input(new csr_crmd_bundle())
        val dmw0 = Input(new csr_dmw_bundle())
        val dmw1 = Input(new csr_dmw_bundle())
        val ppn = Input(UInt((ADDR_WIDTH - PAGE_WIDTH).W))
    })

    val vseg = io.vaddr(31, 29)

    assert(!(io.crmd.pg === 1.U && io.crmd.da === 1.U))
    val pg_mode = io.crmd.pg & ~io.crmd.da
    val da_mode = ~io.crmd.pg & io.crmd.da

    val dmw0_hit = io.dmw0.vseg === vseg
    val dmw1_hit = io.dmw1.vseg === vseg
    val direct_map = pg_mode & (dmw0_hit | dmw1_hit)
    val tlb_map = pg_mode & (~dmw0_hit & ~dmw1_hit)

    // io.paddr := Mux1H(Seq(
    //     da_mode.asBool -> io.vaddr,
    //     direct_map.asBool -> Mux(dmw0_hit, Cat(io.dmw0.pseg , io.vaddr(28,0)), Cat(io.dmw0.pseg , io.vaddr(28,0)))
    // ))
    io.paddr := (
          (Fill(ADDR_WIDTH, da_mode) & io.vaddr)
        | (Fill(ADDR_WIDTH, dmw0_hit) & Cat(io.dmw0.pseg , io.vaddr(28,0)))
        | (Fill(ADDR_WIDTH, dmw1_hit) & Cat(io.dmw1.pseg , io.vaddr(28,0)))
        | (Fill(ADDR_WIDTH, tlb_map) & Cat(io.ppn, io.vaddr(28,0)))
    )
}