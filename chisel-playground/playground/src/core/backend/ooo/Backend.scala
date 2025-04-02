package core

import chisel3._
import chisel3.util._
import core.IssueConfig.ISSUE_WIDTH

class Backend extends Module {
    val io = IO(new Bundle {
        val from = Vec(ISSUE_WIDTH, Flipped(Decoupled(new renaming_to_issue)))
        val out = Vec(ISSUE_WIDTH, Decoupled(UInt(32.W))) 
    })

    // val dispatch = Module(new Dispatch)
    // dispatch.io.in <> io.from(0).bits
    // val alu1rs = Module(new UnorderIssueQueue)
    // val alu2rs = Module(new UnorderIssueQueue)
    // val brurs  = Module(new OrderIssueQueue)
    // val lsurs  = Module(new OrderIssueQueue)
    // val mdurs  = Module(new UnorderIssueQueue)

}