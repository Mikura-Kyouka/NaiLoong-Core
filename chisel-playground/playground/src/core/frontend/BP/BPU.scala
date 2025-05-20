package core
import chisel3._
import chisel3.util._
import core.BpuConfig._

class BtbWay extends Module {
  val io = IO(new Bundle {
    val reqTag = Input(Vec(ISSUE_WIDTH, UInt((32 - 2 - BTB_INDEX_WIDTH).W)))
    val resp = Output(Vec(ISSUE_WIDTH, new BtbEntry))
    val hit = Output(Vec(ISSUE_WIDTH, Bool()))


  })

  val data = RegInit(0.U.asTypeOf(new BtbEntry))
  val tag = RegInit(0.U((32 - 2 - BTB_INDEX_WIDTH).W))
  val valid = RegInit(false.B)

  for(i <- 0 until ISSUE_WIDTH) {
    io.resp(i) := data
    io.hit(i) := tag === io.reqTag(i) && valid
  }
}

class BPU extends Module {
  val io = IO(new Bundle {
    val pc = Input(Vec(ISSUE_WIDTH, UInt(32.W)))
    val taken = Output(Vec(ISSUE_WIDTH, Bool()))
    val target = Output(Vec(ISSUE_WIDTH, UInt(32.W)))
  })

  //  00     01    10     11
  val pff :: pf :: ptt :: pt :: Nil = Enum(4)

  // 三大表项
  val bht = RegInit(VecInit(Seq.fill(1 << INDEX_WIDTH)(0.U.asTypeOf(new BhtEntry))))
  val pht = RegInit(VecInit(Seq.fill(1 << HISTORY_WIDTH)(pf)))
  val btb = Seq.tabulate(1 << BTB_INDEX_WIDTH)(i => Module(new BtbWay))

  // predict
  val phtIdx = Wire(Vec(ISSUE_WIDTH, UInt(INDEX_WIDTH.W)))
  val isTaken = Wire(Vec(ISSUE_WIDTH, Bool()))
  for(i <- 0 until ISSUE_WIDTH) {
    phtIdx(i) := io.pc(i)(INDEX_WIDTH + 1, 2)
  }
  for(i <- 0 until ISSUE_WIDTH) {
    isTaken(i) := pht(bht(phtIdx(i)).history) === pt || pht(bht(phtIdx(i)).history) === ptt
  }

  val btbIdx = Wire(Vec(ISSUE_WIDTH, UInt(BTB_INDEX_WIDTH.W)))
  val btbResp = Wire(Vec(ISSUE_WIDTH, new BtbEntry))
  val btbHit = Wire(Vec(ISSUE_WIDTH, Bool()))
  for(i <- 0 until ISSUE_WIDTH) {
    btbIdx(i) := io.pc(i)(BTB_INDEX_WIDTH + 1, 2)
  }
  for(i <- 0 until ISSUE_WIDTH) {
    val set = btb(btbIdx(i))
    set.io.reqTag(i) := io.pc(i)(32 - 1, BTB_INDEX_WIDTH + 2)
    btbResp(i) := set(i).io.resp(i)
    btbHit(i) := set(i).io.hit(i)
  }
  // 判断是否分支
  for(i <- 0 until ISSUE_WIDTH) {
    io.taken(i) := isTaken(i) && btbHit(i)
    io.target(i) := btbResp(i).target
  }
}