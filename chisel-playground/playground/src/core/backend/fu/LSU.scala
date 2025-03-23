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
  def lr     = "b0001_101".U
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

class StoreQueueEntry extends Bundle {
  val pc = UInt(32.W)
  val prfidx = UInt(7.W)
  val valid = Bool()
}

class DCacheUserBundle extends Bundle {
  val moqidx = UInt(5.W)
  val op     = UInt(7.W)
}

class moqEntry extends Bundle{
  val pc = UInt(32.W)

  val func = UInt(7.W)
  val size = UInt(2.W)
  val op = UInt(7.W)
  val data = UInt(32.W)

  val fdata = UInt(32.W) // forwarding data
  val fmask = UInt(4.W) // forwarding mask

  val isMMIO = Bool()
  val rfWen = Bool()
  val valid = Bool()
  val finished = Bool()
  val storePageFault = Bool()
  val loadPageFault = Bool()
  val loadAddrMisaligned = Bool()
  val storeAddrMisaligned = Bool()
}

class LSUIO extends FunctionUnitIO {
  val wdata = Input(UInt(32.W))
  val axi = new AXI
  // tlb signals

  val uopIn = Input(new RenamedDecodeIO)
  val uopOut = Output(new RenamedDecodeIO)
  val isMMIO = Output(Bool())
}

// class LSU extends Module with HasLSUConst{
//   val io = IO(new LSUIO)
//   val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
//   def access(valid: Bool, src1: UInt, src2: UInt, func: UInt, dtlbPF: Bool): UInt = {
//     io.in.valid := valid
//     io.in.bits.src1 := src1
//     io.in.bits.src2 := src2
//     io.in.bits.func := func
//     io.out.bits
//   }

//   val dmem = io.axi
//   // Gen result
//   val dmemUserOut = dmem.resp.bits.dat
//   val moqidxResp = dmemUserOut.moqidx
//   val moqidxResp = dmem.req

//   // Decode
//   val instr    = io.uopIn.decode.cf.instr
//   val storeReq = valid & LSUOpType.isStore(func)
//   val loadReq  = valid & LSUOpType.isLoad(func)
//   val llReq = valid & LSUOpType.isLL(func)
//   val scReq = valid && LSUOpType.isSC(func)

//   // Atom LL/SC Control Bits
//   val llAddr = WireInit(UInt(32.W), DontCare)
//   val scInvalid = !(src1 === llAddr) && scReq

//   // PF signal from TLB
//   val dtlbPF = WireInit(false.B)

//   val addr = src1 + src2
//   val data = io.uopIn.decode.data.src2
//   val size = func(1, 0)
//   val memop = Wire(UInt(5.W))
//   memop := Cat(
//     false.B, // commitToTLB
//     false.B, // commitToSTQ
//     io.in.valid, // commitToCDB
//     storeReq || (scReq && !scInvalid), // needStore 
//     loadReq || llReq // needLoad
//   )

//   // L/S Queue

//   val storeQueueEnqueue = Wire(Bool())

//   //                           Memory reOrder Queue
//   // ---------------------------------------------------------------------------
//   // |   not used   |   waittlb    |   waitmem    |   dmemreq   |   not used   |
//   // ---------------------------------------------------------------------------
//   //                |              |              |             |
//   //              head            tlb            mem           tail

//   val moq = RegInit(VecInit(Seq.fill(moqSize)(0.U.asTypeOf(new moqEntry))))
//   val moqHeadPtr = RegInit(0.U((log2Up(moqSize)).W))
//   val moqDtlbPtr = RegInit(0.U((log2Up(moqSize)).W))
//   val moqDmemPtr = RegInit(0.U((log2Up(moqSize)).W))
//   val moqTailPtr = RegInit(0.U((log2Up(moqSize)).W))
//   val moqFull = moqHeadPtr === (moqTailPtr - 1.U) // circular queue
//   val moqEmpty = moqHeadPtr === moqTailPtr 
//   val havePendingDtlbReq = moqDtlbPtr =/= moqHeadPtr
//   val havePendingDmemReq = MEMOpID.needLoad(moq(moqDmemPtr).op) && !moq(moqDmemPtr).loadPageFault && !moq(moqDmemPtr).storePageFault && !moq(moqDmemPtr).loadAddrMisaligned && !moq(moqDmemPtr).storeAddrMisaligned && !moq(moqDmemPtr).finished && moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin
//   val havePendingStoreEnq = MEMOpID.needStore(moq(moqDmemPtr).op) && !MEMOpID.needAlu(moq(moqDmemPtr).op) && moq(moqDmemPtr).valid && moq(moqDmemPtr).tlbfin
//   val writebackSelect = Wire(UInt((log2Up(moqSize)).W))
//   writebackSelect := Mux(haveLoadResp, moqidx)  

//   // load queue enqueue
//   val moqEnqeue = io.in.fire
//   when(moqEnqeue){ moqHeadPtr := moqHeadPtr + 1.U }
//   // move moqDtlbPtr
//   val dtlbReqSend = io.dtlb.req.fire // || (!dtlbEnable && moqEnque)
//   when(dtlbReqSend){ moqDtlbPtr := moqDtlbPtr + 1.U }
//   // move moqDmemPtr
//   val arFire = dmem.arvalid && dmem.arready
//   val wFire = (dmem.awvalid && dmem.awready) && (dmem.wvalid && dmem.wready)
//   val moqReqsend = arFire || wFire
//   val nextmoqDmemPtr = WireInit(moqDmemPtr)
//   when(moqReqsend || storeQueueEnqueue){
//     nextmoqDmemPtr := moqDmemPtr + 1.U
//   }
//   moqDmemPtr := nextmoqDmemPtr

//   // load queue dequeue
//   when(io.out.fire){
//     moq(writebackSelect).valid := false.B // TODO: writebackselect
//     moq(writebackSelect).finished := true.B
//   }

//   when((moqTailPtr =/= moqDmemPtr) && !moq(moqTailPtr).valid && ){
//     moqTailPtr := moqTailPtr + 1.U
//     moq(moqTailPtr).valid := false.B
//     moq(moqTailPtr).finished := false.B
//   }
//   // when branch, invalidate insts in wrong branch prediction
//   List.tabulate(moqSize){i => 
//     when(io.uopIn.decode.cf.pc =/= moq(i).pc){
//       moq(i).valid := false.B
//     }
//   }


//   // write data to moq 

//   when(moqEnqeue){
//     moq(moqHeadPtr).pc := io.uopIn.decode.cf.pc
//     moq(moqHeadPtr).func := func
//     moq(moqHeadPtr).size := size
//     moq(moqHeadPtr).op := memop
//     moq(moqHeadPtr).data := genWdata(io.wdata, size)
//   }

//   when(storeQueueEnque){
//     moq(moqDmemPtr).finished := true.B
//     // store inst does not need to access mem until it is commited
//   }

//   //                               Store Queue
//   //              ------------------------------------------------------------
//   // ---> Enqueue |   not used   |   commited   |   retiring   |   retired   |  --> Dequeue
//   //              ------------------------------------------------------------
//   //                             |              |              |
//   //                            head           cmt            req
//   val storeQueue = Reg(Vec(storeQueueSize, new StoreQueueEntry))
//   // Store Queue 
//   val storeHeadPtr    = RegInit(0.U((log2Up(storeQueueSize)+1).W))
//   val storeCmtPtr     = RegInit(0.U((log2Up(storeQueueSize)+1).W))
//   val nextStoreCmtPtr = Wire(UInt((log2Up(storeQueueSize)+1).W))
//   val haveUnconfirmedStore = storeHeadPtr =/= storeCmtPtr
//   val haveUnrequiredStore = storeCmtPtr =/= 0.U && storeQueue(0).valid
//   val haveUnfinishedStore = 0.U =/= storeHeadPtr
//   val storeQueueFull = storeHeadPtr === storeQueueSize.U
//   io.haveUnfinishedStore := haveUnfinishedStore

//   // write data to store queue

//   //-------------------------------------------------------
//   // Load / Store Pipeline
//   //-------------------------------------------------------

//   //-------------------------------------------------------
//   // LSU Stage 1: enqueue
//   // Generate addr, add uop to ls queue
//   //-------------------------------------------------------

//   //-------------------------------------------------------
//   // LSU Stage 2,3,4,5: mem req
//   // Send request to TLB/Cache, and wait for response
//   //-------------------------------------------------------

//   //-------------------------------------------------------
//   // DTLB Access
//   //-------------------------------------------------------
//   // Send request to dtlb
//   when(io.dtlb.resp.fire){
    
//   }


//   //-------------------------------------------------------
//   // Mem Req
//   //-------------------------------------------------------

//   val loadDMemReq  = Wire()
//   val storeDMemReq = Wire()
//   val noDmemReq   = Wire()

//   val storeReadygo = storeDMemReq.valid && (!storeM)
//   val loadReadygo = loadDMemReq.valid && !storeQueueFull
  
//   val memReq = Mux1H(List(
//     loadReadygo -> loadDMemReq,
//     storeReadygo -> storeDMemReq,
//     noDmemReq -> noMemReq
//   ))

//   // Send request to dmem

//   def genWmask(size: UInt): UInt = {
//     LookupTree(sizeEncode, List(
//       0.U -> "b0001".U,
//       1.U -> "b0011".U,
//       2.U -> "b1111".U
//     )) << addr(1, 0)
//   }

//   //-------------------------------------------------------
//   // Mem Resp
//   //-------------------------------------------------------

//   val lsuMMIO = WireInit(false.B)

//   // Store addr forward match
//   // if match, get data from store queue, and mark that inst as resped
//   val dataBackVec = Wire(Vec(4, UInt(32.W)))
//   val dataBack = dataBackVec.asUInt
//   val forwardVec = VecInit(List.tabulate(storeQueueSize){i => {
//     i.U < storeHeadPtr && dmem.araddr(31, 2) === storeQueue(i).paddr(31, 2) && storeQueue(i).valid
//   }})
//   val forwardWmask = List.tabulate(storeQueueSize)(i => storeQueue(i).wmask & Fill(4, forwardVec(i))).foldRight(0.U)((sum, i) => sum | i) // 用于从右到左地对集合中的元素进行递归处理
//   for(j <- 0 to 3){
//     dataBackVec(j) := MuxCase(
//       default = 0.U,
//       mapping = List.tabulate(storeQueueSize)(i =>{
//         (forwardVec(i) -> storeQueue(i).wmask(j), storeQueue(i).data(8*(j+1)-1, 8*j))
//       }).reverse
//     )
//   }
//   // store addr backward match
  
//   // write back to load queue
//   when(dmem.rvalid && dmem.rready && MEMOpID.needLoad(opReg)){
//     moq(moqDmemPtr).fdata := dataBack
//     moq(moqDmemPtr).fmask := forwardWmask
//   }

//   when(dmem.rvalid && dmem.rready){
//     when(MEMOpID.commitToCDB(opResp)){
//       when(MEMOpID.needLoad(opResp)){moq(moqidxResp).data := rdataFwdSel}    
//       moq(moqidxResp).finished := true.B
//     }
//   }
//   //-------------------------------------------------------
//   // LSU Stage 4: Atom and CDB broadcast
//   // Atom ALU gets data and writes its result to temp reg
//   //-------------------------------------------------------



//   val rdataFwdSelVec = Wire(Vec(4, (UInt((4).W)))) // XLEN/8
//   val rdataFwdSel = rdataFwdSelVec.asUInt
//   for(j <- (0 until 4)){
//     //rdataFwdSelVec(j) := Mux() //TODO
//   }
//   val rdata = rdataFwdSel
//   val rdataSel = rdata
//   val rdataPartialLoad = LookupTree(moq(moqidxResp), List(
//       LSUOpType.lb   -> SignExt(rdataSel(7, 0) , 32),
//       LSUOpType.lh   -> SignExt(rdataSel(15, 0), 32),
//       LSUOpType.lw   -> SignExt(rdataSel(31, 0), 32),
//       LSUOpType.lbu  -> ZeroExt(rdataSel(7, 0) , 32),
//       LSUOpType.lhu  -> ZeroExt(rdataSel(15, 0), 32),
//   ))
//   // Commit to CDB
//   io.out.bits := MuxCase(
//     default = rdataPartialLoad, // TODO: exception atom 
//     mapping = List(
//       (moq(writebackSelect).loadPageFault || moq(writebackSelect).storePageFault || moq(writebackSelect).loadAddrMisaligned || moq(writebackSelect).storeAddrMisaligned) -> moq(writebackSelect).vaddr,
//       LSUOpType.isSC(moq(writebackSelect).func) -> !MEMOpID.needStore(moq(writebackSelect).op),
//     )
//   )

//   io.uopOut := DontCare
//   io.isMMIO := moq(writebackSelect).isMMIO
//   io.in.ready := !moqFull 
//   io.out.valid := havePendingCDBCmt || haveLoadResp
//   // assert(!(io.out.valid && !io.out.ready)) // assert(!io.out.valid || io.out.ready)

//   when(io.flush){
//     moqHeadPtr := nextmoqDmemPtr // Head <- nextDmem
//     moqDtlbPtr := nextmoqDmemPtr // Dtlb <- nextDmem
//     for(i <- 0 until moqSize){
//       moq(i).valid := false.B
//       moq(i).tlbin := false.B
//     }
//   }
// }