package core
import chisel3._
import chisel3.util._
import core.BpuConfig._
import core.DualPortBRAM
import core.LSUOpType.sw
import upickle.default

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
  bht0.io.wea := io.wen && (io.waddr(1, 0) === 0.U)
  bht0.io.addra := io.waddr
  bht0.io.dina := io.wdata
  bht0.io.addrb := io.raddr0

  bht1.io.clka := clock
  bht1.io.wea := io.wen && (io.waddr(1, 0) === 1.U)
  bht1.io.addra := io.waddr
  bht1.io.dina := io.wdata
  bht1.io.addrb := io.raddr1

  bht2.io.clka := clock
  bht2.io.wea := io.wen && (io.waddr(1, 0) === 2.U)
  bht2.io.addra := io.waddr
  bht2.io.dina := io.wdata
  bht2.io.addrb := io.raddr2

  bht3.io.clka := clock
  bht3.io.wea := io.wen && (io.waddr(1, 0) === 3.U)
  bht3.io.addra := io.waddr
  bht3.io.dina := io.wdata
  bht3.io.addrb := io.raddr3

  io.rdata0 := bht0.io.doutb
  io.rdata1 := bht1.io.doutb
  io.rdata2 := bht2.io.doutb
  io.rdata3 := bht3.io.doutb
}

class BHTValid extends Module {
  val io = IO(new Bundle {
    val raddr0 = Input(UInt(INDEX_WIDTH.W))
    val rdata0 = Output(Bool())
    val raddr1 = Input(UInt(INDEX_WIDTH.W))
    val rdata1 = Output(Bool())
    val raddr2 = Input(UInt(INDEX_WIDTH.W))
    val rdata2 = Output(Bool())
    val raddr3 = Input(UInt(INDEX_WIDTH.W))
    val rdata3 = Output(Bool())

    val waddr = Input(UInt(INDEX_WIDTH.W))
    val wdata = Input(Bool())
    val wen = Input(Bool())
  })

  val bhtvalid = RegInit(VecInit(Seq.fill(1 << INDEX_WIDTH)(false.B)))

  when(io.wen) {
    bhtvalid(io.waddr) := io.wdata
  }

  io.rdata0 := RegNext(bhtvalid(io.raddr0))
  io.rdata1 := RegNext(bhtvalid(io.raddr1))
  io.rdata2 := RegNext(bhtvalid(io.raddr2))
  io.rdata3 := RegNext(bhtvalid(io.raddr3))
}

class BTBData extends Module {
  val io = IO(new Bundle {
    val raddr0 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata0 = Output(UInt(BTB_DATA_WIDTH.W))
    val raddr1 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata1 = Output(UInt(BTB_DATA_WIDTH.W))
    val raddr2 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata2 = Output(UInt(BTB_DATA_WIDTH.W))
    val raddr3 = Input(UInt(BTB_INDEX_WIDTH.W))
    val rdata3 = Output(UInt(BTB_DATA_WIDTH.W))

    val waddr = Input(UInt(BTB_INDEX_WIDTH.W))
    val wdata = Input(UInt(BTB_DATA_WIDTH.W))
    val wen = Input(Bool())
  })
  // (data, isReturn, isCall) = (32-bit target, 1-bit isReturn, 1-bit isCall)
  val btbdata0 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, BTB_DATA_WIDTH))
  val btbdata1 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, BTB_DATA_WIDTH))
  val btbdata2 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, BTB_DATA_WIDTH))
  val btbdata3 = Module(new DualPortBRAM(BTB_INDEX_WIDTH, BTB_DATA_WIDTH))

  btbdata0.io.clka := clock
  btbdata0.io.wea := io.wen && (io.waddr(1, 0) === 0.U)
  btbdata0.io.addra := io.waddr
  btbdata0.io.dina := io.wdata
  btbdata0.io.addrb := io.raddr0

  btbdata1.io.clka := clock
  btbdata1.io.wea := io.wen && (io.waddr(1, 0) === 1.U)
  btbdata1.io.addra := io.waddr
  btbdata1.io.dina := io.wdata
  btbdata1.io.addrb := io.raddr1

  btbdata2.io.clka := clock
  btbdata2.io.wea := io.wen && (io.waddr(1, 0) === 2.U)
  btbdata2.io.addra := io.waddr
  btbdata2.io.dina := io.wdata
  btbdata2.io.addrb := io.raddr2

  btbdata3.io.clka := clock
  btbdata3.io.wea := io.wen && (io.waddr(1, 0) === 3.U)
  btbdata3.io.addra := io.waddr
  btbdata3.io.dina := io.wdata
  btbdata3.io.addrb := io.raddr3

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
  btbtag0.io.wea := io.wen && (io.waddr(1, 0) === 0.U)
  btbtag0.io.addra := io.waddr
  btbtag0.io.dina := io.wdata
  btbtag0.io.addrb := io.raddr0

  btbtag1.io.clka := clock
  btbtag1.io.wea := io.wen && (io.waddr(1, 0) === 1.U)
  btbtag1.io.addra := io.waddr
  btbtag1.io.dina := io.wdata
  btbtag1.io.addrb := io.raddr1

  btbtag2.io.clka := clock
  btbtag2.io.wea := io.wen && (io.waddr(1, 0) === 2.U)
  btbtag2.io.addra := io.waddr
  btbtag2.io.dina := io.wdata
  btbtag2.io.addrb := io.raddr2

  btbtag3.io.clka := clock
  btbtag3.io.wea := io.wen && (io.waddr(1, 0) === 3.U)
  btbtag3.io.addra := io.waddr
  btbtag3.io.dina := io.wdata
  btbtag3.io.addrb := io.raddr3

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

  val btbvalid = RegInit(VecInit(Seq.fill(1 << BTB_INDEX_WIDTH)(false.B)))

  when(io.wen) {
    btbvalid(io.waddr) := io.wdata
  }

  io.rdata0 := RegNext(btbvalid(io.raddr0))
  io.rdata1 := RegNext(btbvalid(io.raddr1))
  io.rdata2 := RegNext(btbvalid(io.raddr2))
  io.rdata3 := RegNext(btbvalid(io.raddr3))
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
  val bhtvalid = Module(new BHTValid)
  val pht = RegInit(VecInit(Seq.fill(1 << HISTORY_WIDTH)(pf)))
  val btbdata = Module(new BTBData)
  val btbtag = Module(new BTBTag)
  val btbvalid = Module(new BTBValid)

  // ras
  val ras      = RegInit(VecInit(Seq.fill(RAS_DEPTH)(0.U(32.W))))
  val rasTop   = RegInit(0.U(RAS_WIDTH.W))  // 指向“下一次 push”的位置
  def rasPush(addr: UInt) = {
    ras(rasTop) := addr
    rasTop      := rasTop + 1.U
  }
  def rasPop(): UInt = {
    val retAddr = ras(rasTop - 1.U)
    rasTop      := rasTop - 1.U
    retAddr
  }

  // get bht data(pht index) // 1st clock
  val bhtIdx = Wire(Vec(ISSUE_WIDTH, UInt(INDEX_WIDTH.W)))
  for(i <- 0 until ISSUE_WIDTH) {
    bhtIdx(i) := io.pc(i)(INDEX_WIDTH + 1, 2)
  }

  when(io.train.valid) {
    switch(io.train.pc(3, 2)) {
      is(0.U) { bhtIdx(0) := io.train.pc(INDEX_WIDTH + 1, 2) }
      is(1.U) { bhtIdx(1) := io.train.pc(INDEX_WIDTH + 1, 2) }
      is(2.U) { bhtIdx(2) := io.train.pc(INDEX_WIDTH + 1, 2) }
      is(3.U) { bhtIdx(3) := io.train.pc(INDEX_WIDTH + 1, 2) }
    }
  }

  val phtIdx = Wire(Vec(ISSUE_WIDTH, UInt(HISTORY_WIDTH.W)))
  bht.io.raddr0 := bhtIdx(0)
  bht.io.raddr1 := bhtIdx(1)
  bht.io.raddr2 := bhtIdx(2)
  bht.io.raddr3 := bhtIdx(3)

  bhtvalid.io.raddr0 := bhtIdx(0)
  bhtvalid.io.raddr1 := bhtIdx(1)
  bhtvalid.io.raddr2 := bhtIdx(2)
  bhtvalid.io.raddr3 := bhtIdx(3)

  phtIdx(0) := bht.io.rdata0
  phtIdx(1) := bht.io.rdata1
  phtIdx(2) := bht.io.rdata2
  phtIdx(3) := bht.io.rdata3
  dontTouch(phtIdx)

  // get btb tag/valid/data // 1st clock
  val btbIdx = Wire(Vec(ISSUE_WIDTH, UInt(BTB_INDEX_WIDTH.W)))
  dontTouch(btbIdx)

  for(i <- 0 until ISSUE_WIDTH) {
    btbIdx(i) := io.pc(i)(BTB_INDEX_WIDTH + 1, 2)
  }
  val btbTag = Wire(Vec(ISSUE_WIDTH, UInt((32 - 2 - BTB_INDEX_WIDTH).W)))
  val btbValid = Wire(Vec(ISSUE_WIDTH, Bool()))
  val btbData = Wire(Vec(ISSUE_WIDTH, UInt(32.W)))
  val isCall = Wire(Vec(ISSUE_WIDTH, Bool()))
  val isReturn = Wire(Vec(ISSUE_WIDTH, Bool()))

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
  btbData(0) := btbdata.io.rdata0(33, 2) // 32-bit target address
  btbData(1) := btbdata.io.rdata1(33, 2)
  btbData(2) := btbdata.io.rdata2(33, 2)
  btbData(3) := btbdata.io.rdata3(33, 2)
  isReturn(0) := btbdata.io.rdata0(1) // 1: isReturn
  isReturn(1) := btbdata.io.rdata1(1)
  isReturn(2) := btbdata.io.rdata2(1)
  isReturn(3) := btbdata.io.rdata3(1)
  isCall(0) := btbdata.io.rdata0(0) // 0: isCall
  isCall(1) := btbdata.io.rdata1(0)
  isCall(2) := btbdata.io.rdata2(0)
  isCall(3) := btbdata.io.rdata3(0)

  // get pht data // 2nd clock
  val phtData = Wire(Vec(ISSUE_WIDTH, UInt(2.W)))
  for(i <- 0 until ISSUE_WIDTH) {
    phtData(i) := pht(phtIdx(i))  // attention! phtData is valid after 2nd clock
  }
  dontTouch(phtData)

  // predict
  val pcNext = RegNext(io.pc)
  val btbHit = Wire(Vec(ISSUE_WIDTH, Bool()))
  dontTouch(btbHit)
  for(i <- 0 until ISSUE_WIDTH) {
    btbHit(i) := Mux(btbValid(i), (btbTag(i) === pcNext(i)(31, BTB_INDEX_WIDTH + 2)), false.B)
    io.taken(i) := btbHit(i) && phtData(i)(1) // 1: taken
    // io.target(i) := btbData(i) // modified
    io.target(i) := Mux(isReturn(i) && btbHit(i), ras(rasTop - 1.U), btbData(i)) // if isReturn, use ras top

    when(RegNext(io.taken(i) && btbHit(i))) {
      when(RegNext(isCall(i))) {
        val retAddr = RegNext(pcNext(i)) + 4.U   // 先算好打印用
        rasPush(retAddr)

        // printf(p"[RAS PUSH] slot=$i ret=0x${Hexadecimal(retAddr)} oldTop=${rasTop - 1.U}%d newTop=${rasTop}%d\n")
      }.elsewhen(RegNext(isReturn(i))) {
        val popped = rasPop()

        // printf(p"[RAS  POP] slot=$i  tar=0x${Hexadecimal(popped)} oldTop=${rasTop + 1.U}%d newTop=${rasTop}%d\n")
      }
    }
  }

  // train
  // 2nd clock: train btb、bht and pht

  val trainValid    = RegNext(io.train.valid)
  val trainPc       = RegNext(io.train.pc)
  val trainTaken    = RegNext(io.train.taken)
  val trainTarget   = RegNext(io.train.target)
  val trainIsCall   = RegNext(io.train.isCall)
  val trainIsReturn = RegNext(io.train.isReturn)
  val oldHistory    = Wire(UInt(INDEX_WIDTH.W))
  oldHistory := DontCare
  

  btbdata.io.wen   := trainValid && trainTaken
  btbdata.io.waddr := trainPc(BTB_INDEX_WIDTH+1, 2)
  btbdata.io.wdata := Cat(trainTarget, trainIsReturn, trainIsCall) // 32-bit target + 1-bit isReturn + 1-bit isCall

  btbtag.io.wen   := trainValid && trainTaken
  btbtag.io.waddr := trainPc(BTB_INDEX_WIDTH+1, 2)
  btbtag.io.wdata := trainPc(31, BTB_INDEX_WIDTH+2)

  btbvalid.io.wen   := trainValid && trainTaken
  btbvalid.io.waddr := trainPc(BTB_INDEX_WIDTH+1, 2)
  btbvalid.io.wdata := trainTaken 

  bht.io.wen   := trainValid
  bht.io.waddr := trainPc(INDEX_WIDTH+1, 2)
  bht.io.wdata := Cat(trainTarget(HISTORY_WIDTH-2,0), trainTaken)
  bhtvalid.io.wen   := trainValid
  bhtvalid.io.waddr := trainPc(INDEX_WIDTH+1, 2)
  bhtvalid.io.wdata := true.B

  val bhtvalidData = Wire(Bool())
  bhtvalidData := false.B
  switch(trainPc(3, 2)) {
    is(0.U) { bhtvalidData := bhtvalid.io.rdata0 }
    is(1.U) { bhtvalidData := bhtvalid.io.rdata1 }
    is(2.U) { bhtvalidData := bhtvalid.io.rdata2 }
    is(3.U) { bhtvalidData := bhtvalid.io.rdata3 }
  }

  switch(trainPc(3, 2)) {
    is(0.U) { oldHistory := bht.io.rdata0 }
    is(1.U) { oldHistory := bht.io.rdata1 }
    is(2.U) { oldHistory := bht.io.rdata2 }
    is(3.U) { oldHistory := bht.io.rdata3 }
  }

  when (trainValid && bhtvalidData) {
    val idx = oldHistory
    switch (pht(idx)) {
      is(pff) { pht(idx) := Mux(trainTaken, pf, pff) }
      is(pf)  { pht(idx) := Mux(trainTaken, pt, pff) }
      is(pt)  { pht(idx) := Mux(trainTaken, ptt, pf) }
      is(ptt) { pht(idx) := Mux(trainTaken, ptt, pt) }
    }
  }
}