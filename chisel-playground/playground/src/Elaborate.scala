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
    new core.ALU(), 
    args = Array(
      "--throw-on-first-error",
      "--split-verilog",
      "--target-dir=./build"
    ), 
    firtoolOptions
  )
  circt.stage.ChiselStage.emitSystemVerilogFile(
    new core.IDU(), 
    args = Array(
      "--throw-on-first-error",
      "--split-verilog",
      "--target-dir=./build"
    ), 
    firtoolOptions
  )
    circt.stage.ChiselStage.emitSystemVerilogFile(
    new core.RegRenaming(), 
    args = Array(
      "--throw-on-first-error",
      "--split-verilog",
      "--target-dir=./build"
    ), 
    firtoolOptions
  )
    circt.stage.ChiselStage.emitSystemVerilogFile(
    new core.ROB(), 
    args = Array(
      "--throw-on-first-error",
      "--split-verilog",
      "--target-dir=./build"
    ), 
    firtoolOptions
  )
}
