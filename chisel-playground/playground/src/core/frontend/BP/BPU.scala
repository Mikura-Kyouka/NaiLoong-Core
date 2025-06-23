package core
import chisel3._
import chisel3.util._
import core.BpuConfig._
import core.DualPortBRAM

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

  val bht0 = Module(new DualPortBRAM(INDEX_WIDTH, HISTORY_WIDTH))
  val bht1 = Module(new DualPortBRAM(INDEX_WIDTH, HISTORY_WIDTH))
  val bht2 = Module(new DualPortBRAM(INDEX_WIDTH, HISTORY_WIDTH))
  val bht3 = Module(new DualPortBRAM(INDEX_WIDTH, HISTORY_WIDTH))

  bht0.io.clka := clock
  bht0.io.ena := true.B
  bht0.io.addra := io.raddr0
  bht0.io.wea := false.B
  bht0.io.dina := 0.U

  bht0.io.clkb := clock
  bht0.io.enb := true.B
  bht0.io.web := io.wen && (io.waddr(1, 0) === 0.U)
  bht0.io.addrb := io.waddr
  bht0.io.dinb := io.wdata

  bht1.io.clka := clock
  bht1.io.ena := true.B
  bht1.io.addra := io.raddr1
  bht1.io.wea := false.B
  bht1.io.dina := 0.U

  bht1.io.clkb := clock
  bht1.io.enb := true.B
  bht1.io.web := io.wen && (io.waddr(1, 0) === 1.U)
  bht1.io.addrb := io.waddr
  bht1.io.dinb := io.wdata

  bht2.io.clka := clock
  bht2.io.ena := true.B
  bht2.io.addra := io.raddr2
  bht2.io.wea := false.B
  bht2.io.dina := 0.U

  bht2.io.clkb := clock
  bht2.io.enb := true.B
  bht2.io.web := io.wen && (io.waddr(1, 0) === 2.U)
  bht2.io.addrb := io.waddr
  bht2.io.dinb := io.wdata

  bht3.io.clka := clock
  bht3.io.ena := true.B
  bht3.io.addra := io.raddr3
  bht3.io.wea := false.B
  bht3.io.dina := 0.U

  bht3.io.clkb := clock
  bht3.io.enb := true.B
  bht3.io.web := io.wen && (io.waddr(1, 0) === 3.U)
  bht3.io.addrb := io.waddr
  bht3.io.dinb := io.wdata

  io.rdata0 := bht0.io.doutb
  io.rdata1 := bht1.io.doutb
  io.rdata2 := bht2.io.doutb
  io.rdata3 := bht3.io.doutb
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

  val btbdata0 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32))
  val btbdata1 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32))
  val btbdata2 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32))
  val btbdata3 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32))

  btbdata0.io.clka := clock
  btbdata0.io.ena := true.B
  btbdata0.io.addra := io.raddr0
  btbdata0.io.wea := false.B
  btbdata0.io.dina := 0.U

  btbdata0.io.clkb := clock
  btbdata0.io.enb := true.B
  btbdata0.io.web := io.wen && (io.waddr(1, 0) === 0.U)
  btbdata0.io.addrb := io.waddr
  btbdata0.io.dinb := io.wdata

  btbdata1.io.clka := clock
  btbdata1.io.ena := true.B
  btbdata1.io.addra := io.raddr1
  btbdata1.io.wea := false.B
  btbdata1.io.dina := 0.U

  btbdata1.io.clkb := clock
  btbdata1.io.enb := true.B
  btbdata1.io.web := io.wen && (io.waddr(1, 0) === 1.U)
  btbdata1.io.addrb := io.waddr
  btbdata1.io.dinb := io.wdata

  btbdata2.io.clka := clock
  btbdata2.io.ena := true.B
  btbdata2.io.addra := io.raddr2
  btbdata2.io.wea := false.B
  btbdata2.io.dina := 0.U

  btbdata2.io.clkb := clock
  btbdata2.io.enb := true.B
  btbdata2.io.web := io.wen && (io.waddr(1, 0) === 2.U)
  btbdata2.io.addrb := io.waddr
  btbdata2.io.dinb := io.wdata

  btbdata3.io.clka := clock
  btbdata3.io.ena := true.B
  btbdata3.io.addra := io.raddr3
  btbdata3.io.wea := false.B
  btbdata3.io.dina := 0.U

  btbdata3.io.clkb := clock
  btbdata3.io.enb := true.B
  btbdata3.io.web := io.wen && (io.waddr(1, 0) === 3.U)
  btbdata3.io.addrb := io.waddr
  btbdata3.io.dinb := io.wdata

  io.rdata0 := btbdata0.io.doutb
  io.rdata1 := btbdata1.io.doutb
  io.rdata2 := btbdata2.io.doutb
  io.rdata3 := btbdata3.io.doutb
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

  val btbtag0 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32 - 2 - BTB_INDEX_WIDTH))
  val btbtag1 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32 - 2 - BTB_INDEX_WIDTH))
  val btbtag2 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32 - 2 - BTB_INDEX_WIDTH))
  val btbtag3 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 32 - 2 - BTB_INDEX_WIDTH))

  btbtag0.io.clka := clock
  btbtag0.io.ena := true.B
  btbtag0.io.addra := io.raddr0
  btbtag0.io.wea := false.B
  btbtag0.io.dina := 0.U

  btbtag0.io.clkb := clock
  btbtag0.io.enb := true.B
  btbtag0.io.web := io.wen && (io.waddr(1, 0) === 0.U)
  btbtag0.io.addrb := io.waddr
  btbtag0.io.dinb := io.wdata

  btbtag1.io.clka := clock
  btbtag1.io.ena := true.B
  btbtag1.io.addra := io.raddr1
  btbtag1.io.wea := false.B
  btbtag1.io.dina := 0.U

  btbtag1.io.clkb := clock
  btbtag1.io.enb := true.B
  btbtag1.io.web := io.wen && (io.waddr(1, 0) === 1.U)
  btbtag1.io.addrb := io.waddr
  btbtag1.io.dinb := io.wdata

  btbtag2.io.clka := clock
  btbtag2.io.ena := true.B
  btbtag2.io.addra := io.raddr2
  btbtag2.io.wea := false.B
  btbtag2.io.dina := 0.U

  btbtag2.io.clkb := clock
  btbtag2.io.enb := true.B
  btbtag2.io.web := io.wen && (io.waddr(1, 0) === 2.U)
  btbtag2.io.addrb := io.waddr
  btbtag2.io.dinb := io.wdata

  btbtag3.io.clka := clock
  btbtag3.io.ena := true.B
  btbtag3.io.addra := io.raddr3
  btbtag3.io.wea := false.B
  btbtag3.io.dina := 0.U

  btbtag3.io.clkb := clock
  btbtag3.io.enb := true.B
  btbtag3.io.web := io.wen && (io.waddr(1, 0) === 3.U)
  btbtag3.io.addrb := io.waddr
  btbtag3.io.dinb := io.wdata

  io.rdata0 := btbtag0.io.doutb
  io.rdata1 := btbtag1.io.doutb
  io.rdata2 := btbtag2.io.doutb
  io.rdata3 := btbtag3.io.doutb
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

  val btbvalid0 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 1))
  val btbvalid1 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 1))
  val btbvalid2 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 1))
  val btbvalid3 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, 1))

  btbvalid0.io.clka := clock
  btbvalid0.io.ena := true.B
  btbvalid0.io.addra := io.raddr0
  btbvalid0.io.wea := false.B
  btbvalid0.io.dina := 0.U

  btbvalid0.io.clkb := clock
  btbvalid0.io.enb := true.B
  btbvalid0.io.web := io.wen && (io.waddr(1, 0) === 0.U)
  btbvalid0.io.addrb := io.waddr
  btbvalid0.io.dinb := io.wdata

  btbvalid1.io.clka := clock
  btbvalid1.io.ena := true.B
  btbvalid1.io.addra := io.raddr1
  btbvalid1.io.wea := false.B
  btbvalid1.io.dina := 0.U

  btbvalid1.io.clkb := clock
  btbvalid1.io.enb := true.B
  btbvalid1.io.web := io.wen && (io.waddr(1, 0) === 1.U)
  btbvalid1.io.addrb := io.waddr
  btbvalid1.io.dinb := io.wdata

  btbvalid2.io.clka := clock
  btbvalid2.io.ena := true.B
  btbvalid2.io.addra := io.raddr2
  btbvalid2.io.wea := false.B
  btbvalid2.io.dina := 0.U

  btbvalid2.io.clkb := clock
  btbvalid2.io.enb := true.B
  btbvalid2.io.web := io.wen && (io.waddr(1, 0) === 2.U)
  btbvalid2.io.addrb := io.waddr
  btbvalid2.io.dinb := io.wdata

  btbvalid3.io.clka := clock
  btbvalid3.io.ena := true.B
  btbvalid3.io.addra := io.raddr3
  btbvalid3.io.wea := false.B
  btbvalid3.io.dina := 0.U

  btbvalid3.io.clkb := clock
  btbvalid3.io.enb := true.B
  btbvalid3.io.web := io.wen && (io.waddr(1, 0) === 3.U)
  btbvalid3.io.addrb := io.waddr
  btbvalid3.io.dinb := io.wdata

  io.rdata0 := btbvalid0.io.doutb.asBool
  io.rdata1 := btbvalid1.io.doutb.asBool
  io.rdata2 := btbvalid2.io.doutb.asBool
  io.rdata3 := btbvalid3.io.doutb.asBool
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