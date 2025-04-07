import core.DCacheConfig
object Elaborate extends App {
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
    new core.DCache()(new DCacheConfig(totalSize = 4 * 16, ways = 1)),
    args = Array(
      "--throw-on-first-error",
      "--split-verilog",
      "--target-dir=./build"
    ), 
    firtoolOptions
  )
}
