package core
import chisel3._
import chisel3.util._
import chisel3.util.experimental._

import utils._

class UnpipeLSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(32.W))
  val dmem = new AXI
  val robRetire = Input(Bool()) // Store insts
  val complete = Output(Bool()) // tell ROB to commit inst
  val isMMIO = Output(Bool())
  val dtlbPF = Output(Bool()) // TODO: refactor it for new backend
  val loadAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
  val storeAddrMisaligned = Output(Bool()) // TODO: refactor it for new backend
}

class UnpipelinedLSU extends Module with HasLSUConst {
  val io = IO(new UnpipeLSUIO)
  val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
  def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, dtlbPF: Bool): UInt = {
    this.valid := valid
    this.src1 := src1
    this.src2 := src2
    this.func := func
    dtlbPF := io.dtlbPF
    io.out.bits
  }
  val lsExecUnit = Module(new LSExecUnit)
  io.dtlbPF := lsExecUnit.io.dtlbPF
  io.complete := lsExecUnit.io.complete
  lsExecUnit.io.robRetire := io.robRetire

  // loadReq -> send req to mem and wait for resp 
  // storeReq -> wait resp from ROB then send req and wait for resp
  val storeReq = valid & LSUOpType.isStore(func)
  val loadReq  = valid & LSUOpType.isLoad(func)
  val atomReq  = valid & LSUOpType.isAtom(func)
  val llReq   = valid & LSUOpType.isLL(func)
  val scReq   = valid & LSUOpType.isSC(func)

  // Atom LL/SC Control Bits
  val setLL = Wire(Bool())
  val setLLVal = Wire(Bool())
  val setLLAddr = Wire(UInt(32.W))
  val ll = WireInit(Bool(), false.B)
  val llAddr = WireInit(UInt(32.W), DontCare)

  val scInvalid = (src1 =/= llAddr || !ll) && scReq

  // PF signal from TLB
  val dtlbFinish = WireInit(false.B)
  val dtlbPF = WireInit(false.B)
  val dtlbEnable = WireInit(false.B)

  // LSU control FSM state
  val s_idle :: s_exec :: s_ll :: s_sc :: Nil = Enum(4)

  // LSU control FSM
  val state = RegInit(s_idle)
  val addr = RegNext(src1 + src2, state === s_idle)
  // StoreQueue
  // TODO: inst fence needs storeQueue to be finished
  // val enableStoreQueue = EnableStoreQueue // StoreQueue is disabled for page fault detection
  // if(enableStoreQueue){
  //   val storeQueue = Module(new Queue(new StoreQueueEntry, 4))
  //   storeQueue.io.enq.valid := state === s_idle && storeReq
  //   storeQueue.io.enq.bits.src1 := src1
  //   storeQueue.io.enq.bits.src2 := src2
  //   storeQueue.io.enq.bits.wdata := io.wdata
  //   storeQueue.io.enq.bits.func := func
  //   storeQueue.io.deq.ready := lsExecUnit.io.out.fire
  // }

  lsExecUnit.io.in.valid     := false.B
  lsExecUnit.io.out.ready    := DontCare
  lsExecUnit.io.in.bits.src1 := DontCare
  lsExecUnit.io.in.bits.src2 := DontCare
  lsExecUnit.io.in.bits.func := DontCare
  lsExecUnit.io.wdata        := DontCare
  io.out.valid               := false.B
  io.in.ready                := false.B

  // 
  state := MuxLookup(state, s_idle)(List(
    s_idle -> MuxCase(s_idle, Seq(
      llReq -> s_ll,
      scReq -> Mux(scInvalid, s_idle, s_sc),
      valid -> s_exec
    )), // check in order
    s_exec -> Mux(io.out.fire, s_idle, s_exec),
    s_ll -> Mux(lsExecUnit.io.out.fire, s_idle, s_ll),
    s_sc -> Mux(lsExecUnit.io.out.fire, s_idle, s_sc)
  ))

  switch (state) {
    is(s_idle){ // calculate address
      lsExecUnit.io.in.valid     := false.B
      lsExecUnit.io.out.ready    := DontCare
      lsExecUnit.io.in.bits.src1 := DontCare
      lsExecUnit.io.in.bits.src2 := DontCare
      lsExecUnit.io.in.bits.func := DontCare
      lsExecUnit.io.wdata        := DontCare
      io.in.ready                := false.B || scInvalid
      io.out.valid               := false.B || scInvalid

      lsExecUnit.io.in.valid     := io.in.valid && !atomReq
      lsExecUnit.io.out.ready    := io.out.ready
      lsExecUnit.io.in.bits.src1 := src1 + src2
      lsExecUnit.io.in.bits.src2 := DontCare
      lsExecUnit.io.in.bits.func := func
      lsExecUnit.io.wdata        := io.wdata
      io.in.ready                := lsExecUnit.io.out.fire || scInvalid
      io.out.valid               := lsExecUnit.io.out.valid  || scInvalid
    }

    is(s_exec){
      lsExecUnit.io.in.valid     := true.B
      lsExecUnit.io.out.ready    := io.out.ready
      lsExecUnit.io.in.bits.src1 := addr
      lsExecUnit.io.in.bits.src2 := DontCare
      lsExecUnit.io.in.bits.func := func
      lsExecUnit.io.wdata        := io.wdata
      io.in.ready                := lsExecUnit.io.out.fire
      io.out.valid               := lsExecUnit.io.out.valid
    }

    is(s_ll){
      lsExecUnit.io.in.valid     := true.B
      lsExecUnit.io.out.ready    := io.out.ready
      lsExecUnit.io.in.bits.src1 := src1
      lsExecUnit.io.in.bits.src2 := DontCare
      lsExecUnit.io.in.bits.func := LSUOpType.lw
      lsExecUnit.io.wdata        := DontCare
      io.in.ready                := lsExecUnit.io.out.fire
      io.out.valid               := lsExecUnit.io.out.fire
      when(lsExecUnit.io.out.fire){ printf("[LL]\n")}
    }

    is(s_sc){
      lsExecUnit.io.in.valid     := true.B
      lsExecUnit.io.out.ready    := io.out.ready
      lsExecUnit.io.in.bits.src1 := src1
      lsExecUnit.io.in.bits.src2 := DontCare
      lsExecUnit.io.in.bits.func := LSUOpType.sw
      lsExecUnit.io.wdata        := io.wdata
      io.in.ready                := lsExecUnit.io.out.fire
      io.out.valid               := lsExecUnit.io.out.fire
      when(lsExecUnit.io.out.fire){ printf("[SC] \n") }
    }
  }
  when(dtlbPF || io.loadAddrMisaligned || io.storeAddrMisaligned){
    state := s_idle
    io.out.valid := true.B
    io.in.ready := true.B
  }
  when(io.out.fire){
    printf("[LSU-AGU] state %x in.v %x in.r %x\n", state, io.in.valid, io.in.ready)
  }
  // controled by FSM
  // io.in.ready := lsExecUnit.io.in.ready
  // lsExecUnit.io.wdata := io.wdata
  // io.out.valid := lsExecUnit.io.out.valid

  //Set LL/SC bits
  setLL := io.out.fire && (llReq || scReq)
  setLLVal := llReq
  setLLAddr := src1

  io.dmem <> lsExecUnit.io.dmem
  io.out.bits := Mux(scReq, scInvalid, lsExecUnit.io.out.bits)

  val lsuMMIO = WireInit(false.B)
  val mmioReg = RegInit(false.B)
  when (!mmioReg) { mmioReg := lsuMMIO }
  when (io.out.valid) { mmioReg := false.B }
  io.isMMIO := mmioReg && io.out.valid

  io.loadAddrMisaligned := lsExecUnit.io.loadAddrMisaligned
  io.storeAddrMisaligned := lsExecUnit.io.storeAddrMisaligned
}

class LSExecUnit extends Module {
  val io = IO(new UnpipeLSUIO)

  val (valid, addr, func) = (io.in.valid, io.in.bits.src1, io.in.bits.func) // src1 is used as address
  def access(valid: Bool, addr: UInt, func: UInt): UInt = {
    this.valid := valid
    this.addr := addr
    this.func := func
    io.out.bits
  }

  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(1:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U  //1111
    )) << addr(1, 0)
  }
  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(4, data(7, 0)),
      "b01".U -> Fill(2, data(15, 0)),
      "b10".U -> data
    ))
  }

  val dmem = io.dmem
  val addrLatch = RegNext(addr)
  val isStore = valid && LSUOpType.isStore(func)
  val partialLoad = !isStore && (func =/= LSUOpType.lw)

  val s_idle :: s_wait_tlb :: s_load_req :: s_load_resp :: s_wait_retire :: s_store_req :: s_store_resp :: Nil = Enum(7)
  val state = RegInit(s_idle)

  val dtlbFinish = WireInit(false.B)
  val dtlbPF = WireInit(false.B)
  val dtlbEnable = WireInit(false.B)

  io.dtlbPF := dtlbPF

  state := MuxLookup(state, s_idle)(List(
    s_idle -> Mux(io.in.valid, Mux(dtlbPF, s_wait_tlb, Mux(isStore, Mux(io.robRetire, s_store_req, s_wait_retire), s_load_req)), s_idle),
    s_wait_tlb -> Mux(dtlbFinish, Mux(dtlbPF, s_idle, Mux(isStore, s_store_req, s_load_req)), s_wait_tlb),
    s_load_req -> Mux(dmem.arvalid && dmem.arready, s_load_resp, s_load_req),
    s_load_resp -> Mux(dmem.rvalid && dmem.rready, s_idle, s_load_resp),
    s_wait_retire -> Mux(io.robRetire, s_store_req, s_wait_retire),
    s_store_req -> Mux(dmem.awvalid && dmem.awready && dmem.wvalid && dmem.wready, s_store_resp, s_store_req),
    s_store_resp -> Mux(dmem.bvalid && dmem.bready, s_idle, s_store_resp)
  ))

  val size = func(1,0)
  val reqWdata = genWdata(io.wdata, size)
  val reqWmask = genWmask(addr, size)
  dmem := DontCare
  dmem.araddr := addr
  dmem.arvalid := state === s_load_req
  dmem.rready := true.B
  dmem.awaddr := addr
  dmem.awvalid := state === s_store_req
  dmem.wvalid := state === s_store_req
  dmem.bready := true.B
  dmem.wdata := reqWdata
  dmem.wstrb := reqWmask

  io.out.valid := Mux( dtlbPF && state =/= s_idle || io.loadAddrMisaligned || io.storeAddrMisaligned, true.B, Mux(state === s_load_resp, dmem.rvalid, dmem.bvalid))
  io.in.ready := (state === s_idle) || dtlbPF

  val rdata = dmem.rdata
  val rdataLatch = RegNext(rdata)

  val rdataSel = LookupTree(addrLatch(1, 0), List(
    "b00".U -> rdataLatch(31, 0),
    "b01".U -> rdataLatch(31, 8),
    "b10".U -> rdataLatch(31, 16),
    "b11".U -> rdataLatch(31, 24)
  ))

  val rdataPartialLoad = LookupTree(func, List(
      LSUOpType.lb   -> SignExt(rdataSel(7, 0) , 32),
      LSUOpType.lh   -> SignExt(rdataSel(15, 0), 32),
      LSUOpType.lw   -> SignExt(rdataSel(31, 0), 32),
      LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , 32),
      LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), 32)
  ))

  val addrAligned = LookupTree(func(1,0), List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (addr(0) === 0.U),   //h
    "b10".U   -> (addr(1,0) === 0.U)  //w
  ))

  io.out.bits := Mux(partialLoad, rdataPartialLoad, rdata)

  io.isMMIO := DontCare
  io.complete := (state === s_load_resp && dmem.rvalid) || (state === s_store_resp && dmem.bvalid)
  io.loadAddrMisaligned :=  valid && !isStore && !addrAligned
  io.storeAddrMisaligned := valid && isStore && !addrAligned
  when(io.loadAddrMisaligned || io.storeAddrMisaligned){
    printf("misaligned addr detected\n")
  }
}

class AligendUnpipelinedLSU extends Module{
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(Output(new PipelineConnectIO)))
    val out = Decoupled(new FuOut)
  })
  val lsu = Module(new UnpipelinedLSU)
  lsu.io := DontCare
  lsu.io.in.bits.src1 := io.in.bits.src1
  lsu.io.in.bits.src2 := Mux(io.in.bits.ctrl.src2Type === 1.U, io.in.bits.imm, io.in.bits.src2)
  lsu.io.in.bits.func := io.in.bits.ctrl.fuOpType
  io.out.bits.data := lsu.io.out.bits
  io.out.bits.robIdx := io.in.bits.robIdx
  
  lsu.io.in.valid := io.in.valid
  io.in.ready := lsu.io.in.ready
  io.out.valid := lsu.io.out.valid
  lsu.io.out.ready := io.out.ready
} 
