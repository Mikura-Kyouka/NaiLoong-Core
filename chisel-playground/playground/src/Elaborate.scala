import core.DCacheConfig
import core.GenCtrl
import core.Core

object Elaborate extends App {
  val parser = new scopt.OptionParser[Config]("Elaborate") {
    opt[String]("target-dir")
      .action((x, c) => c.copy(targetDir = x))
      .text("指定输出目录")
    opt[Boolean]("use-diff")
      .action((x, c) => c.copy(useDiff = x))
      .text("是否启用 DiffTest")
    opt[Boolean]("use-simu")
      .action((x, c) => c.copy(useSimu = x))
      .text("是否使用仿真用 IP")
  }
  case class Config(
    targetDir: String = "build",
    useDiff: Boolean = true,
    useSimu: Boolean = true
  )
  val config = parser.parse(args, Config()).getOrElse(Config())
  GenCtrl.USE_DIFF = config.useDiff
  GenCtrl.USE_SIMU = config.useSimu
  
  val firtoolOptions = Array(
    "--lowering-options=" + List(
      // make yosys happy
      // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _)
  )
  circt.stage.ChiselStage.emitSystemVerilogFile(
    // new core.DCache()(new DCacheConfig(totalSize = 4 * 16, ways = 1)), 
    new Core(),
    args = Array(
      "--throw-on-first-error",
      "--split-verilog",
      "--target-dir=./build"
    ), 
    firtoolOptions
  )
}
