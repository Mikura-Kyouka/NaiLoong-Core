package core
import chisel3._
import chisel3.util._
import core.BpuConfig._

class BtbWay extends Module {
  val io = IO(new Bundle {
    val reqTag = Input(Vec(ISSUE_WIDTH, UInt((32 - 2 - BTB_INDEX_WIDTH).W)))
    val resp = Output(Vec(ISSUE_WIDTH, new BtbEntry))
    val hit = Output(Vec(ISSUE_WIDTH, Bool()))

    val train = Input(new BranchTrainInfo)
  })

  val data = RegInit(0.U.asTypeOf(new BtbEntry))
  val tag = RegInit(0.U((32 - 2 - BTB_INDEX_WIDTH).W))
  val valid = RegInit(false.B)

  for(i <- 0 until ISSUE_WIDTH) {
    io.resp(i) := data
    io.hit(i) := tag === io.reqTag(i) && valid
  }

  when(io.train.valid) {
    data.target := io.train.target
    tag := io.train.pc(32 - 1, BTB_INDEX_WIDTH + 2)
    valid := true.B
  }
}

/*
          ┌─────────┐                                    
┌────────>│   BTB   ├─────────> taken := pht(1) && BTBHit
│         └─────────┘                ▲                   
│                                    │                          
│                                    │                   
│         ┌─────────┐  index    ┌────┴────┐              
PC───────>│   BHT   ├──────────>│   PHT   │              
          └─────────┘           └─────────┘        
*/      

object PhrStatus {
  val pff = "00b".U(2.W) // strongly not taken
  val pf  = "01b".U(2.W) // weakly not taken
  val pt  = "11b".U(2.W) // weakly taken
  val ptt = "10b".U(2.W) // strongly taken
}

class BPU extends Module {
  val io = IO(new Bundle {
    val pc = Input(Vec(ISSUE_WIDTH, UInt(32.W)))
    val taken = Output(Vec(ISSUE_WIDTH, Bool()))
    val target = Output(Vec(ISSUE_WIDTH, UInt(32.W)))

    val train = Input(new BranchTrainInfo)
  })
  import PhrStatus._

  // 三大表项
  val bht = RegInit(VecInit(Seq.fill(1 << INDEX_WIDTH)(0.U.asTypeOf(new BhtEntry))))
  val pht = RegInit(VecInit(Seq.fill(1 << HISTORY_WIDTH)(pf)))
  val btb = VecInit.tabulate(1 << BTB_INDEX_WIDTH)(i => Module(new BtbWay).io)

  // predict
  val phtIdx = Wire(Vec(ISSUE_WIDTH, UInt(INDEX_WIDTH.W)))
  val isTaken = Wire(Vec(ISSUE_WIDTH, Bool()))
  for(i <- 0 until ISSUE_WIDTH) {
    phtIdx(i) := io.pc(i)(INDEX_WIDTH + 1, 2)
  }
  for(i <- 0 until ISSUE_WIDTH) {
    isTaken(i) := pht(bht(phtIdx(i)).history)(1)
  }

  val btbIdx = Wire(Vec(ISSUE_WIDTH, UInt(BTB_INDEX_WIDTH.W)))
  val btbResp = Wire(Vec(ISSUE_WIDTH, new BtbEntry))
  val btbHit = Wire(Vec(ISSUE_WIDTH, Bool()))
  for(i <- 0 until ISSUE_WIDTH) {
    btbIdx(i) := io.pc(i)(BTB_INDEX_WIDTH + 1, 2)
  }
  for(i <- 0 until ISSUE_WIDTH) {
    val set = btb(btbIdx(i))
    set.reqTag(i) := io.pc(i)(32 - 1, BTB_INDEX_WIDTH + 2)
    btbResp(i) := set.resp(i)
    btbHit(i) := set.hit(i)
  }
  // 判断是否预测分支发生
  for(i <- 0 until ISSUE_WIDTH) {
    io.taken(i) := isTaken(i) && btbHit(i)
    io.target(i) := btbResp(i).target
  }

  when(io.train.valid) {
    val idx = io.train.pc(INDEX_WIDTH + 1, 2)
    val bhtEntry = bht(idx)
    val phtEntry = pht(bhtEntry.history)

    switch(phtEntry) {
      is(pff) {
        pht(bhtEntry.history) := Mux(io.train.taken, pf, pff)
      }
      is(pf) {
        pht(bhtEntry.history) := Mux(io.train.taken, pt, pff)
      }
      is(pt) {
        pht(bhtEntry.history) := Mux(io.train.taken, ptt, pf)
      }
      is(ptt) {
        pht(bhtEntry.history) := Mux(io.train.taken, ptt, pt)
      }
    }
    bhtEntry.history := Cat(bhtEntry.history(HISTORY_WIDTH - 2, 0), io.train.taken)
  }
  btb(io.train.pc(BTB_INDEX_WIDTH + 1, 2)).train := io.train
}