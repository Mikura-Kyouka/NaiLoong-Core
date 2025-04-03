package core

import chisel3._
import chisel3.util._
import IssueConfig._
import utils.PipelineConnect

class Backend extends Module {
    val io = IO(new Bundle {
        val from = Vec(4, Flipped(Decoupled(new renaming_to_issue)))
        val out = Vec(ISSUE_WIDTH, Decoupled(UInt(32.W))) 
        // commit inst
        val cmtInstr = Flipped(Valid(new commit_inst_info))
        // retire inst 
        val rtrInstr = Flipped(Valid(new retire_inst_info))
    })
    //////////////////////
    ////Dispatch Stage////
    //////////////////////
    val dispatch = Module(new Dispatch)
    dispatch.io.in <> io.from
    val alu1rs = Module(new UnorderIssueQueue)
    val alu2rs = Module(new UnorderIssueQueue)
    val mdurs  = Module(new UnorderIssueQueue)
    val lsurs  = Module(new OrderIssueQueue)
    val brurs  = Module(new OrderIssueQueue)
    PipelineConnect(dispatch.io.out(0), alu1rs.io.in, alu1rs.io.out.fire, false.B)
    PipelineConnect(dispatch.io.out(1), alu2rs.io.in, alu2rs.io.out.fire, false.B)
    PipelineConnect(dispatch.io.out(2), mdurs.io.in,  mdurs.io.out.fire,  false.B)
    PipelineConnect(dispatch.io.out(3), lsurs.io.in,  lsurs.io.out.fire,  false.B)
    PipelineConnect(dispatch.io.out(4), brurs.io.in,  brurs.io.out.fire,  false.B)
    
    // Connect retire inst 
    val busyreg = RegInit(VecInit(Seq.fill(PHYS_REG_NUM)(false.B)))
    for(i <- 0 until 4) { // TODO: parameterize FETCH_WIDTH
        val dest = io.from(i).bits.dest
        when(io.from(i).valid && io.from(i).ready) {
            busyreg(dest) := true.B
        }
    }
    when(io.cmtInstr.valid) {
        busyreg(io.cmtInstr.bits.inst.dest) := false.B
    }

    // Connect busy signal
    alu1rs.io.busyreg := busyreg
    alu2rs.io.busyreg := busyreg
    mdurs.io.busyreg := busyreg
    lsurs.io.busyreg := busyreg
    brurs.io.busyreg := busyreg

    val payloadram = Module(new PayloadRAM)
    alu1rs.io.pram_read <> payloadram.io.read(0)
    alu2rs.io.pram_read <> payloadram.io.read(1)
    mdurs.io.pram_read <> payloadram.io.read(2)
    lsurs.io.pram_read <> payloadram.io.read(3)
    brurs.io.pram_read <> payloadram.io.read(4)
    payloadram.io.write.dest := io.rtrInstr.bits.preg
    payloadram.io.write.pram_data := io.rtrInstr.bits.data
    payloadram.io.write.valid := io.rtrInstr.valid

    //////////////////////
    //// Issue  Stage ////
    //////////////////////

    // Instantiate functional module
    // val alu1 = Module(new ALU)
    // val alu2 = Module(new ALU)
    // val mdu = Module(new MDU)
    // val lsu = Module(new UnpipelinedLSU)
    // val bru = Module(new ALU) // TODO

    // PipelineConnect(alu1rs.io.out, alu1.io.in, alu1.io.out.fire, false.B)
    // PipelineConnect(alu2rs.io.out, alu2.io.in, alu2.io.out.fire, false.B)
    // PipelineConnect(mdurs.io.out,  mdu.io.in,  mdu.io.out.fire,  false.B)
    // PipelineConnect(lsurs.io.out,  lsu.io.in,  lsu.io.out.fire,  false.B)
    // PipelineConnect(brurs.io.out,  bru.io.in,  bru.io.out.fire,  false.B)
}