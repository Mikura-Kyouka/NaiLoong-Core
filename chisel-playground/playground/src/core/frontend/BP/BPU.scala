package core
import chisel3._
import chisel3.util._
import core.BpuConfig._

class BHT extends Module {
  val io = IO(new Bundle {
    val raddr0 = Input(UInt(INDEX_WIDTH.W))
    val rdata0 = Output(UInt(HISTORY_WIDTH.W))
    val raddr1 = Input(UInt(INDEX_WIDTH.W))
    val rdata1 = Output(UInt(HISTORY_WIDTH.W))
    val raddr2 = Input(UInt(INDEX_WIDTH.W))
    val rdata2 = Output(UInt(HISTORY_WIDTH.W))
    val raddr3 = Input(UInt(INDEX_WIDTH.W))
    val rdata3 = Output(UInt(HISTORY_WIDTH.W))

    val waddr = Input(UInt(INDEX_WIDTH.W))
    val wdata = Input(UInt(HISTORY_WIDTH.W))
    val wen = Input(Bool())
  })

  val mem = SyncReadMem(1 << INDEX_WIDTH, UInt(HISTORY_WIDTH.W))
  when (io.wen) {
    mem.write(io.waddr, io.wdata)
  }
  io.rdata0 := mem.read(io.raddr0, true.B)
  io.rdata1 := mem.read(io.raddr1, true.B)
  io.rdata2 := mem.read(io.raddr2, true.B)
  io.rdata3 := mem.read(io.raddr3, true.B)
}

class BTBData extends Module {
  val io = IO(new Bundle {
    val raddr0 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata0 = Output(UInt(32.W))
    val raddr1 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata1 = Output(UInt(32.W))
    val raddr2 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata2 = Output(UInt(32.W))
    val raddr3 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata3 = Output(UInt(32.W))

    val waddr = Input(UInt(BTB_INDEX_WIDTH.W))
    val wdata = Input(UInt(32.W))
    val wen = Input(Bool())
  })
  val mem = SyncReadMem(1 << BTB_INDEX_WIDTH, UInt(32.W))
  when (io.wen) {
    mem.write(io.waddr, io.wdata)
  }
  io.rdata0 := mem.read(io.raddr0, true.B)
  io.rdata1 := mem.read(io.raddr1, true.B)
  io.rdata2 := mem.read(io.raddr2, true.B)
  io.rdata3 := mem.read(io.raddr3, true.B)
}

class BTBTag extends Module {
  val io = IO(new Bundle {
    val raddr0 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata0 = Output(UInt((32 - 2 - BTB_INDEX_WIDTH).W))
    val raddr1 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata1 = Output(UInt((32 - 2 - BTB_INDEX_WIDTH).W))
    val raddr2 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata2 = Output(UInt((32 - 2 - BTB_INDEX_WIDTH).W))
    val raddr3 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata3 = Output(UInt((32 - 2 - BTB_INDEX_WIDTH).W))

    val waddr = Input(UInt(BTB_INDEX_WIDTH.W))
    val wdata = Input(UInt((32 - 2 - BTB_INDEX_WIDTH).W))
    val wen = Input(Bool())
  })
  val mem = SyncReadMem(1 << BTB_INDEX_WIDTH, UInt((32 - 2 - BTB_INDEX_WIDTH).W))
  when (io.wen) {
    mem.write(io.waddr, io.wdata)
  }
  io.rdata0 := mem.read(io.raddr0, true.B)
  io.rdata1 := mem.read(io.raddr1, true.B)
  io.rdata2 := mem.read(io.raddr2, true.B)
  io.rdata3 := mem.read(io.raddr3, true.B)
}

class BTBValid extends Module {
  val io = IO(new Bundle {
    val raddr0 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata0 = Output(Bool())
    val raddr1 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata1 = Output(Bool())
    val raddr2 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata2 = Output(Bool())
    val raddr3 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata3 = Output(Bool())

    val waddr = Input(UInt(BTB_INDEX_WIDTH.W))
    val wdata = Input(Bool())
    val wen = Input(Bool())
  })
  val mem = SyncReadMem(1 << BTB_INDEX_WIDTH, Bool())
  when (io.wen) {
    mem.write(io.waddr, io.wdata)
  }
  io.rdata0 := mem.read(io.raddr0, true.B)
  io.rdata1 := mem.read(io.raddr1, true.B)
  io.rdata2 := mem.read(io.raddr2, true.B)
  io.rdata3 := mem.read(io.raddr3, true.B)
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
BTB和BHT是SyncReadMem，PHT是Reg，第二拍出结果   
*/      

class BPU extends Module {
  val io = IO(new Bundle {
    val pc = Input(Vec(ISSUE_WIDTH, UInt(32.W)))
    val taken = Output(Vec(ISSUE_WIDTH, Bool()))
    val target = Output(Vec(ISSUE_WIDTH, UInt(32.W)))

    val train = Input(new BranchTrainInfo)
  })

  //  00     01    10     11
  val pff :: pf :: ptt :: pt :: Nil = Enum(4)

  // 三大表项
  val bht = Module(new BHT)
  val pht = RegInit(VecInit(Seq.fill(1 << HISTORY_WIDTH)(pf)))
  val btbdata = Module(new BTBData)
  val btbtag = Module(new BTBTag)
  val btbvalid = Module(new BTBValid)

  // get bht data(pht index) // 1st clock
  val bhtIdx = Wire(Vec(ISSUE_WIDTH, UInt(INDEX_WIDTH.W)))
  for(i <- 0 until ISSUE_WIDTH) {
    bhtIdx(i) := io.pc(i)(INDEX_WIDTH + 1, 2)
  }
  bhtIdx(0) := Mux(io.train.valid, io.train.pc(INDEX_WIDTH + 1, 2), io.pc(0)(INDEX_WIDTH + 1, 2))  // FIXME: shoud not like this
  val phtIdx = Wire(Vec(ISSUE_WIDTH, UInt(HISTORY_WIDTH.W)))
  bht.io.raddr0 := bhtIdx(0)
  bht.io.raddr1 := bhtIdx(1)
  bht.io.raddr2 := bhtIdx(2)
  bht.io.raddr3 := bhtIdx(3)
  phtIdx(0) := bht.io.rdata0
  phtIdx(1) := bht.io.rdata1
  phtIdx(2) := bht.io.rdata2
  phtIdx(3) := bht.io.rdata3

  // get btb tag/valid/data // 1st clock
  val btbIdx = Wire(Vec(ISSUE_WIDTH, UInt(BTB_INDEX_WIDTH.W)))
  for(i <- 0 until ISSUE_WIDTH) {
    btbIdx(i) := io.pc(i)(BTB_INDEX_WIDTH + 1, 2)
  }
  val btbTag = Wire(Vec(ISSUE_WIDTH, UInt((32 - 2 - BTB_INDEX_WIDTH).W)))
  val btbValid = Wire(Vec(ISSUE_WIDTH, Bool()))
  val btbData = Wire(Vec(ISSUE_WIDTH, UInt(32.W)))
  btbtag.io.raddr0 := btbIdx(0)
  btbtag.io.raddr1 := btbIdx(1)
  btbtag.io.raddr2 := btbIdx(2)
  btbtag.io.raddr3 := btbIdx(3)
  btbTag(0) := btbtag.io.rdata0
  btbTag(1) := btbtag.io.rdata1
  btbTag(2) := btbtag.io.rdata2
  btbTag(3) := btbtag.io.rdata3
  btbvalid.io.raddr0 := btbIdx(0)
  btbvalid.io.raddr1 := btbIdx(1)
  btbvalid.io.raddr2 := btbIdx(2)
  btbvalid.io.raddr3 := btbIdx(3)
  btbValid(0) := btbvalid.io.rdata0
  btbValid(1) := btbvalid.io.rdata1
  btbValid(2) := btbvalid.io.rdata2
  btbValid(3) := btbvalid.io.rdata3
  btbdata.io.raddr0 := btbIdx(0)
  btbdata.io.raddr1 := btbIdx(1)
  btbdata.io.raddr2 := btbIdx(2)
  btbdata.io.raddr3 := btbIdx(3)
  btbData(0) := btbdata.io.rdata0
  btbData(1) := btbdata.io.rdata1
  btbData(2) := btbdata.io.rdata2
  btbData(3) := btbdata.io.rdata3

  // get pht data // 2nd clock
  val phtData = Wire(Vec(ISSUE_WIDTH, UInt(2.W)))
  for(i <- 0 until ISSUE_WIDTH) {
    phtData(i) := pht(phtIdx(i))  // attention! phtData is valid after 2nd clock
  }

  // predict
  val pcNext = RegNext(io.pc)
  val btbHit = Wire(Vec(ISSUE_WIDTH, Bool()))
  for(i <- 0 until ISSUE_WIDTH) {
    btbHit(i) := btbValid(i) && (btbTag(i) === pcNext(i)(31, BTB_INDEX_WIDTH + 2))
    io.taken(i) := btbHit(i) && phtData(i)(1) // 1: taken
    io.target(i) := btbData(i)
  }

  // train
  // 2nd clock: train btb、bht and pht

  val trainValid    = RegNext(io.train.valid)
  val trainPc       = RegNext(io.train.pc)
  val trainTaken    = RegNext(io.train.taken)
  val trainTarget   = RegNext(io.train.target)
  val oldHistory = bht.io.rdata0 // attention! bht.io.rdata0 is valid after 2nd clock

  btbdata.io.wen   := trainValid && trainTaken
  btbdata.io.waddr := trainPc(BTB_INDEX_WIDTH+1, 2)
  btbdata.io.wdata := trainTarget

  btbtag.io.wen   := trainValid && trainTaken
  btbtag.io.waddr := trainPc(BTB_INDEX_WIDTH+1, 2)
  btbtag.io.wdata := trainPc(31, BTB_INDEX_WIDTH+2)

  btbvalid.io.wen   := trainValid && trainTaken
  btbvalid.io.waddr := trainPc(BTB_INDEX_WIDTH+1, 2)
  btbvalid.io.wdata := trainTaken 

  bht.io.wen   := trainValid
  bht.io.waddr := trainPc(INDEX_WIDTH+1, 2)
  bht.io.wdata := Cat(oldHistory(HISTORY_WIDTH-2,0), trainTaken)

  when (trainValid) {
    val idx = oldHistory
    switch (pht(idx)) {
      is(pff) { pht(idx) := Mux(trainTaken, pf, pff) }
      is(pf)  { pht(idx) := Mux(trainTaken, pt, pff) }
      is(pt)  { pht(idx) := Mux(trainTaken, ptt, pf) }
      is(ptt) { pht(idx) := Mux(trainTaken, ptt, pt) }
    }
  }
}