package core
import chisel3._
import chisel3.util._

import utils._

object LSUOpType {
  def lb   = "b0000000".U
  def lh   = "b0000001".U
  def lw   = "b0000010".U
  def lbu  = "b0000100".U
  def lhu  = "b0000101".U
  def sb   = "b0001000".U
  def sh   = "b0001001".U
  def sw   = "b0001010".U

  def ll   = "b0100000".U 
  def sc   = "b0100001".U
  def isAdd(func: UInt) = func(6)
  def isAtom(func: UInt): Bool = func(5)
  def isStore(func: UInt): Bool = func(3)
  def isLoad(func: UInt): Bool = !isStore(func) & !isAtom(func)
  def isLL(func: UInt): Bool = func === ll
  def isSC(func: UInt): Bool = func === sc
  // def isAMO(func: UInt): Bool = isAtom(func) && !isLR(func) && !isSC(func)

  // def needMemRead(func: UInt): Bool = isLoad(func) || isAMO(func) || isLR(func)
  // def needMemWrite(func: UInt): Bool = isStore(func) || isAMO(func) || isSC(func)

  def atomW = "010".U
  def atomD = "011".U
}

object MEMOpID {
  def idle   = "b0000_000".U
  def load   = "b0001_001".U
  def store  = "b0001_010".U
  def storec = "b0010_010".U //store commit
  def ll     = "b0001_101".U
  def sc     = "b0001_110".U
  def tlb    = "b0100_001".U

  def needLoad(memop: UInt) = memop(0)
  def needStore(memop: UInt) = memop(1)
  def commitToCDB(memop: UInt) = memop(3)
  def commitToSTQ(memop: UInt) = memop(4)
  def commitToTLB(memop: UInt) = memop(5)
}

trait HasLSUConst {
  val moqSize = 8
  val storeQueueSize = 8
}

class storeQueueEntry extends Bundle {
  val pc       = UInt(32.W)
  // val prfidx   = UInt(prfAddrWidth.W) // for debug
  // val brMask   = UInt(checkpointSize.W)
  val wmask    = UInt(4.W) // for store queue forwarding
  val vaddr    = UInt(32.W)
  val paddr    = UInt(32.W)
  val func     = UInt(7.W)
  val size     = UInt(2.W)
  val op       = UInt(7.W)
  val data     = UInt(32.W)
  val isMMIO   = Bool()
  val valid    = Bool()
}

class moqEntry extends Bundle{
  val pc       = UInt(32.W)
  // val prfidx   = UInt(prfAddrWidth.W) // FIXME:
  // val brMask   = UInt(checkpointSize.W)
  // val stMask   = UInt(robSize.W)
  val vaddr    = UInt(32.W) // VAddrBits for debug
  val paddr    = UInt(32.W) // PAddrBits 
  val func     = UInt(7.W)
  val size     = UInt(2.W)
  val op       = UInt(7.W)
  val data     = UInt(32.W)
  val fdata    = UInt(32.W) // forwarding data
  val fmask    = UInt(4.W) // forwarding mask
  val rfWen    = Bool()
  val isMMIO   = Bool()
  val valid    = Bool()
  val tlbfin   = Bool()
  val finished = Bool()
  val rollback = Bool()
  val loadPageFault  = Bool()
  val storePageFault  = Bool()
  val loadAddrMisaligned  = Bool()
  val storeAddrMisaligned = Bool()
}

class LSU extends Module with HasLSUConst {
  val io = IO(new Bundle{
    val in = Flipped(Decoupled(Output(new PipelineConnectIO)))
    // dtlb
    val addr_trans_out = Output(new AddrTrans)
    val addr_trans_in = Input(new AddrTrans)
    // dmem
    val dmemReq = Decoupled(new reqBundle)
    val dmemResp = Flipped(Decoupled(new respBundle))
    val flush = Input(Bool())
    val scommit = Input(Bool())
    val out = Decoupled(new FuOut)
  })
  val (valid, src1, src2, func) = (io.in.bits.valid, io.in.bits.src1, Mux(io.in.bits.ctrl.src2Type === 1.U, io.in.bits.imm, io.in.bits.src2), io.in.bits.ctrl.fuOpType)
  dontTouch(io.in.bits.pc)
  io.in.ready := io.out.ready
  io.out := DontCare
  io.addr_trans_out := DontCare
  io.dmemReq := DontCare
  // val dmem = io.dmem
  val opResp = 0.U // default load op, for debug
  val moqidxResp = 0.U

  val storeReq = valid & LSUOpType.isStore(func)
  val loadReq = valid & LSUOpType.isLoad(func)
  val findLoadAddrMisaligned = Wire(Bool())
  val findStoreAddrMisaligned = Wire(Bool())

  val addr = io.in.bits.src1 + io.in.bits.imm
  val wdata = io.in.bits.src2
  val size = func(1, 0)
  // L/S Queue

  val storeQueueEnqueue = Wire(Bool())
  //     
  //                           Memory reOrder Queue
  // ---------------------------------------------------------------------------
  // |   not used   |   wait       |   waitmem    |   waittlb   |   not used   |
  // ---------------------------------------------------------------------------
  //                |              |              |             |
  //              tail            dmem            tlb          head
  val moq = RegInit(VecInit(Seq.fill(moqSize)(0.U.asTypeOf(new moqEntry))))
  // Memory reOrder Queue contains Load, Store and TLB request
  // Store insts will access TLB before its result being commited to CDB
  // moq should be called 'loadStoreQueue' or 'memReOrderQueue', in some ways
  val moqHeadPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqDtlbPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqDmemPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqTailPtr  = RegInit(0.U((log2Up(moqSize)).W))
  val moqFull = moqHeadPtr === (moqTailPtr - 1.U)
  val moqEmpty = moqHeadPtr === moqTailPtr
  val havePendingDtlbReq = moqDtlbPtr =/= moqHeadPtr
  val havePendingDmemReq = MEMOpID.needLoad(moq(moqDmemPtr).op) && !moq(moqDmemPtr).finished && moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin
  val havePendingStoreEnq = MEMOpID.needStore(moq(moqDmemPtr).op) && moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin
  val dmemReqFrommoq = havePendingDmemReq || havePendingStoreEnq // 是否有需要从 moq 中发起对 dmem 的请求
  val haveLoadResp = io.dmemResp.fire && MEMOpID.commitToCDB(opResp) && moq(moqidxResp).valid //FIXIT: to use non blocking
  val havePendingCDBCmt = (0 until moqSize).map(i => moq(i).finished && moq(i).valid).reduce(_ | _)
  val pendingCDBCmtSelect = PriorityEncoder(VecInit((0 until moqSize).map(i => moq(i).finished && moq(i).valid))) // 给出了当前可以提交到 CDB 的 moq entry 的编号，优先选择队列中最前面的已完成且有效的指令进行提交
  val writebackSelect = Wire(UInt(log2Up(moqSize).W))
  writebackSelect := Mux(haveLoadResp, moqidxResp, pendingCDBCmtSelect)

  // load queue enqueue
  val moqEnqueue = io.in.valid // FIXME:
  when(moqEnqueue){moqHeadPtr := moqHeadPtr + 1.U}
  // move moqDtlbptr
  // 如果有等待的Dtlb请求，DtlbPtr下个周期增加1（Dtlb一个周期后总能返回数据）
  when(havePendingDtlbReq) {moqDtlbPtr := moqDtlbPtr + 1.U}
  // move moqDmemptr
  val moqReqsend = io.dmemReq.fire // FIXME: dmem.req.fire && MEMOpID.commitToCDB(opReq) // F
  dontTouch(io.dmemReq.fire) // Debug Only
  val nextmoqDmemPtr = WireInit(moqDmemPtr)
  when(moqReqsend || storeQueueEnqueue){ // 已被dmem接收/入了store queue
    nextmoqDmemPtr := moqDmemPtr + 1.U
  }
  moqDmemPtr := nextmoqDmemPtr
  
  // load queue enqueue 
  when(io.out.fire){
    moq(writebackSelect).valid := false.B
    moq(writebackSelect).finished := true.B
  }

  when((moqTailPtr =/= moqDmemPtr) && !moq(moqTailPtr).valid && moq(moqTailPtr).finished){
    moqTailPtr := moqTailPtr + 1.U
    moq(moqTailPtr).valid := false.B
    moq(moqTailPtr).finished := false.B
  }

  // flush
  when(io.flush) {
    for (i <- 0 until moqSize) {
      moq(i).valid := false.B
    }
  }

  // write data to moq
  val vaddrIsMMIO = addr(31, 16) === "hbfaf".U
  val paddrIsMMIO = true.B // io.dtlb.resp.bits.rdata(31, 16) === "hbfaf".U

  when(moqEnqueue){
    moq(moqHeadPtr).pc := io.in.bits.pc
    moq(moqHeadPtr).vaddr := addr
    moq(moqHeadPtr).paddr := addr
    moq(moqHeadPtr).func := func
    moq(moqHeadPtr).size := size
    // moq(moqHeadPtr).op := memop
    moq(moqHeadPtr).data := genWdata(wdata, size)
    moq(moqHeadPtr).fdata := 0.U
    moq(moqHeadPtr).fmask := 0.U
    // moq(moqHeadPtr).asrc := io.wdata // FIXIT
    moq(moqHeadPtr).rfWen := io.in.bits.ctrl.rfWen
    moq(moqHeadPtr).isMMIO := vaddrIsMMIO // FIXIT
    moq(moqHeadPtr).valid := true.B
    moq(moqHeadPtr).tlbfin := false.B // tlbfinished
    moq(moqHeadPtr).finished := false.B
    moq(moqHeadPtr).rollback := false.B
    moq(moqHeadPtr).loadPageFault := false.B
    moq(moqHeadPtr).storePageFault := false.B
    moq(moqHeadPtr).loadAddrMisaligned := findLoadAddrMisaligned
    moq(moqHeadPtr).storeAddrMisaligned := findStoreAddrMisaligned
  }

  when(storeQueueEnqueue){
    moq(moqDmemPtr).finished := true.B
    // store inst does not need to access mem until it is commited
  }

  //                               Store Queue
  //              ------------------------------------------------------------
  // ---> Enqueue |   not used   |   commited   |   retiring   |   retired   |  --> Dequeue
  //              ------------------------------------------------------------
  //                             |              |              |
  //                            head           cmt            req
  val storeQueue = Reg(Vec(storeQueueSize, new storeQueueEntry)) 
  // Store Queue contains store insts that have finished TLB lookup stage
  // There are 2 types of store insts in this queue: ROB-commited (retired) / CDB-commited (commited)
  // CDB-commited insts have already gotten their paddr from TLB,
  // but whether these insts will be canceled is still pending for judgement.
  // ROB-commited insts are those insts already retired from ROB
  // val storeAlloc   = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val storeHeadPtr    = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val storeCmtPtr     = RegInit(0.U((log2Up(storeQueueSize)+1).W))
  val nextStoreCmtPtr = Wire(UInt((log2Up(storeQueueSize)+1).W))
  val haveUnconfirmedStore = storeHeadPtr =/= storeCmtPtr
  val haveUnrequiredStore = storeCmtPtr =/= 0.U && storeQueue(0).valid
  val haveUnfinishedStore = 0.U =/= storeHeadPtr
  val storeQueueFull = storeHeadPtr === storeQueueSize.U
  // io.haveUnfinishedStore := haveUnfinishedStore // FIXME: 

  // alloc a slot when a store tlb request is sent
  // val storeQueueAlloc = dmem.req.fire && MEMOpID.commitToCDB(opReq) && MEMOpID.needStore(opReq)
  // after a store inst get its paddr from TLB, add it to store queue
  val dtlbRespUser = 0.U // io.dtlb.resp.bits.user.get.asTypeOf(new DCacheUserBundle)
  val tlbRespStoreEnq = false.B
  storeQueueEnqueue := havePendingStoreEnq && !storeQueueFull

  val storeQueueConfirm = io.scommit // TODO: Argo only support 1 scommit / cycle
  // when a store inst actually writes data to dmem, mark it as `waiting for dmem resp`
  val storeQueueReqsend = io.dmemReq.fire //  && MEMOpID.commitToSTQ(opReq)
  // when dmem try to commit to store queue, i.e. dmem report a write op is finished, dequeue
  // FIXIT: in current case, we can always assume a store is succeed after req.fire
  // therefore storeQueueDequeue is not necessary
  val storeQueueDequeue = storeQueueReqsend
  when(storeQueueDequeue){
    // storeQueue := Cat(storeQueue(0), storeQueue(storeQueueSize-1, 1))
    // 将队列中第 1 ~ N 项依次赋值给第 0 ~ N-1 项，所有元素整体前移一位
    List.tabulate(storeQueueSize - 1)(i => {
      storeQueue(i) := storeQueue(i+1)
    })
    // 队尾的 entry 标记为无效，表示该位置现在是空的
    storeQueue(storeQueueSize-1).valid := false.B
  }

  // move storeCmtPtr ptr
  nextStoreCmtPtr := storeCmtPtr
  // 如果发生出队（且不是跳过无效项），但没有新的 store 指令退休，则提交指针向前移动（减1），因为队列长度减少了。
  when(storeQueueDequeue && !storeQueueConfirm){nextStoreCmtPtr := storeCmtPtr - 1.U}
  // 如果没有出队（或只是跳过无效项），但有新的 store 指令退休，则提交指针向后移动（加1），因为队列长度增加了。
  when(!storeQueueDequeue && storeQueueConfirm){nextStoreCmtPtr := storeCmtPtr + 1.U}
  storeCmtPtr := nextStoreCmtPtr
 
  // move storeHeadPtr ptr
  // 如果发生出队，且本周期没有入队，则队列长度减少，storeHeadPtr 向前移动（减1）。
  when(storeQueueDequeue && !storeQueueEnqueue){storeHeadPtr := storeHeadPtr - 1.U}
  // 如果没有出队，但有入队，则队列长度增加，storeHeadPtr 向后移动（加1）。
  when(!storeQueueDequeue && storeQueueEnqueue){storeHeadPtr := storeHeadPtr + 1.U}
  // 保证分支恢复后，Store Queue 只保留已经提交的 store 指令，后面的全部丢弃。
  val flushStoreHeadPtr = PriorityMux(
    (nextStoreCmtPtr === 0.U) +: (0 until storeQueueSize).map(i => {
      PopCount(VecInit((0 to i).map(j => storeQueue(j).valid))) === nextStoreCmtPtr
    }),
    (0 to storeQueueSize).map(i => i.U)
  )
  when(io.flush){storeHeadPtr := flushStoreHeadPtr}

  val storeQueueEnqPtr = Mux(storeQueueDequeue, storeHeadPtr - 1.U, storeHeadPtr)
  val storeQueueEnqSrcPick = moqDmemPtr
  dontTouch(storeQueueEnqueue)
  when(storeQueueEnqueue){
    storeQueue(storeQueueEnqPtr).pc := moq(storeQueueEnqSrcPick).pc
    storeQueue(storeQueueEnqPtr).wmask := genWmask(moq(storeQueueEnqSrcPick).vaddr, moq(moqDmemPtr).size)
    storeQueue(storeQueueEnqPtr).vaddr := moq(storeQueueEnqSrcPick).vaddr
    storeQueue(storeQueueEnqPtr).paddr := moq(moqDmemPtr).paddr
    storeQueue(storeQueueEnqPtr).func := moq(storeQueueEnqSrcPick).func
    storeQueue(storeQueueEnqPtr).size := moq(storeQueueEnqSrcPick).size
    storeQueue(storeQueueEnqPtr).op := moq(storeQueueEnqSrcPick).op
    storeQueue(storeQueueEnqPtr).data := moq(storeQueueEnqSrcPick).data
    storeQueue(storeQueueEnqPtr).isMMIO := moq(moqDmemPtr).isMMIO
    storeQueue(storeQueueEnqPtr).valid := true.B
  }

  //-------------------------------------------------------
  // Load / Store Pipeline
  //-------------------------------------------------------

  //-------------------------------------------------------
  // LSU Stage 1: enqueue
  // Generate addr, add uop to ls queue
  //-------------------------------------------------------

  // Exception check, fix mop
  // Misaligned exception generation
  val addrAligned = LookupTree(size, List(
    "b00".U   -> true.B,              //b
    "b01".U   -> (addr(0) === 0.U),   //h
    "b10".U   -> (addr(1,0) === 0.U)  //w
  ))
  findLoadAddrMisaligned  := valid && !storeReq && !addrAligned
  findStoreAddrMisaligned := valid && storeReq && !addrAligned

  //-------------------------------------------------------
  // LSU Stage 2,3,4,5: mem req
  // Send request to TLB/Cache, and wait for response
  //-------------------------------------------------------


  //-------------------------------------------------------
  // DTLB Access
  //-------------------------------------------------------
  // Send request to dtlb
  val dtlbMoqIdx = moqDtlbPtr
  io.addr_trans_out.trans_en := io.in.fire // 
  io.addr_trans_out.vaddr := addr
  moq(moqDtlbPtr).paddr := io.addr_trans_in.paddr
  moq(moqDtlbPtr).tlbfin := true.B // tlbfinished
  moq(moqDtlbPtr).isMMIO := paddrIsMMIO
  moq(moqDtlbPtr).loadPageFault := false.B
  moq(moqDtlbPtr).storePageFault := false.B

  //-------------------------------------------------------
  // Mem Req
  //-------------------------------------------------------

  // Send request to dmem 

  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(1:0)
      "b01".U -> 0x3.U, //0011
      "b10".U -> 0xf.U, //1111
    )) << addr(1, 0)
  }

  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> Fill(4, data(7, 0)),
      "b01".U -> Fill(2, data(15, 0)),
      "b10".U -> data(31, 0)
    ))
  }

  when(haveUnrequiredStore){
    io.dmemReq.bits.addr := storeQueue(moqDmemPtr).paddr
    io.dmemReq.bits.size := storeQueue(0.U).size
    io.dmemReq.bits.wdata := genWdata(storeQueue(0.U).data, storeQueue(0.U).size)
    io.dmemReq.bits.wmask := genWmask(storeQueue(0.U).paddr, storeQueue(0.U).size)
    io.dmemReq.bits.cmd := 1.U
    io.dmemReq.valid := havePendingDmemReq
  }.elsewhen(havePendingDmemReq){
    io.dmemReq.bits.addr := moq(moqDmemPtr).paddr
    io.dmemReq.bits.addr := moq(moqDmemPtr).size
    io.dmemReq.bits.wdata := genWdata(moq(moqDmemPtr).data, moq(moqDmemPtr).size)
    io.dmemReq.bits.wmask := genWmask(moq(moqDmemPtr).paddr, moq(moqDmemPtr).size)
    io.dmemReq.bits.cmd := 0.U
    io.dmemReq.valid := havePendingDmemReq
  }
  io.dmemResp.ready := true.B

  //-------------------------------------------------------
  // Mem Resp
  //-------------------------------------------------------

  // Load Queue dequeue
  // In-order dequeue
  // If it is a valid store inst, add it to store queue
  // If an inst is marked as `finished`, it will be commited to CDB in the next cycle

  // MMIO check 
  

  // Store addr forward match
  // If match, get data from store queue, and mark that inst as resped
  val dataBackVec = Wire(Vec(4, UInt(8.W)))
  val dataBack = dataBackVec.asUInt
  val forwardVec = VecInit(List.tabulate(storeQueueSize)(i => {
    i.U < storeHeadPtr && 
    io.dmemReq.bits.addr(31, 2) === storeQueue(i).paddr(31, 2) && storeQueue(i).valid
  }))
  val forwardWmask = List.tabulate(storeQueueSize)(i => storeQueue(i).wmask & Fill(4, forwardVec(i))).foldRight(0.U)((sum, i) => sum | i)

  for(j <- 0 to 3){
    dataBackVec(j) := MuxCase(
      // default = dmem.resp.bits.rdata(8*(j+1)-1, 8*j),
      default = 0.U,
      mapping = List.tabulate(storeQueueSize)(i => {
        (forwardVec(i) && storeQueue(i).wmask(j), storeQueue(i).data(8*(j+1)-1, 8*j))
      }).reverse
    )
  }

  // val storeNeedRollback 

  // write back to load queue
  // when(dmem.req.fire && MEMOpID.needLoad(opReq)){
  //   moq(moqDmemPtr).fdata := dataBack
  //   moq(moqDmemPtr).fmask := forwardWmask
  // }

  // val rdataFwdSelVec = Wire(Vec(4, (UInt(4.W))))
  // val rdataFwdSel = rdataFwdSelVec.asUInt
  // for(j <- (0 until (XLEN/8))){
  //   rdataFwdSelVec(j) := Mux(moq(moqidxResp).fmask(j), moq(moqidxResp).fdata(8*(j+1)-1, 8*j), dmem.resp.bits.rdata(8*(j+1)-1, 8*j))
  // }
  //-------------------------------------------------------
  // LSU Stage 4: Atom and CDB broadcast
  // Atom ALU gets data and writes its result to temp reg
  //-------------------------------------------------------

  // Load Data Selection
  // val rdata = rdataFwdSel
  // val rdataSel = LookupTree(moq(moqidxResp).vaddr(2, 0), List(
  //   "b000".U -> rdata(63, 0),
  //   "b001".U -> rdata(63, 8),
  //   "b010".U -> rdata(63, 16),
  //   "b011".U -> rdata(63, 24),
  //   "b100".U -> rdata(63, 32),
  //   "b101".U -> rdata(63, 40),
  //   "b110".U -> rdata(63, 48),
  //   "b111".U -> rdata(63, 56)
  // ))
  // val rdataPartialLoad = LookupTree(moq(moqidxResp).func, List(
  //     LSUOpType.lb   -> SignExt(rdataSel(7, 0) , XLEN),
  //     LSUOpType.lh   -> SignExt(rdataSel(15, 0), XLEN),
  //     LSUOpType.lw   -> SignExt(rdataSel(31, 0), XLEN),
  //     LSUOpType.ld   -> SignExt(rdataSel(63, 0), XLEN),
  //     LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , XLEN),
  //     LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), XLEN),
  //     LSUOpType.lwu  -> ZeroExt(rdataSel(31, 0), XLEN)
  // ))

  // // Commit to CDB
  // io.out.bits := MuxCase(
  //     default = rdataPartialLoad,
  //     mapping = List(
  //       (moq(writebackSelect).loadPageFault || moq(writebackSelect).storePageFault || moq(writebackSelect).loadAddrMisaligned || moq(writebackSelect).storeAddrMisaligned) -> moq(writebackSelect).vaddr,
  //       LSUOpType.isSC(moq(writebackSelect).func) -> !MEMOpID.needStore(moq(writebackSelect).op),
  //       LSUOpType.isAtom(moq(writebackSelect).func) -> atomDataPartialLoad
  //     )
  // )

  // io.isMMIO := moq(writebackSelect).isMMIO
  
  // io.in.ready := !moqFull 
  // io.out.valid := havePendingCDBCmt || haveLoadResp
  // assert(!(io.out.vlaid && !io.out.ready))

  // when(io.flush){
  //   moqHeadPtr := nextmoqDmemPtr
  //   moqDtlbPtr := nextmoqDmemPtr
  //   for(i <- 0 to (moqSize - 1)){
  //     moq(i).valid := false.B
  //     moq(i).tlbfin := false.B
  //   }
  // }
}