package sturdopt.optimizations

import sturdopt.util.CfgRelatedMethods
import swam.ModuleLoader
import swam.binary.ModuleParser
import swam.syntax.{Func, Module}
import sturdy.fix.StackConfig
import sturdy.language.wasm.abstractions.{CfgConfig, CfgNode, ControlFlow}
import sturdy.language.wasm.analyses.{ConstantAnalysis, FixpointConfig, WasmConfig}
import sturdy.language.wasm.generic.FrameData
import sturdy.language.wasm.generic.InstLoc.InFunction

object DeadcodeOptimization:

  // TODO Add deadLabel handling and completely remove deadFunctions
  def eliminateDeadcode(mod: Module): Module =
    val stackConfig = StackConfig.StackedStates()
    val interp = new ConstantAnalysis.Instance(FrameData.empty, Iterable.empty, WasmConfig(fix = FixpointConfig(iter = sturdy.fix.iter.Config.Innermost(stackConfig))))
    val cfg = ConstantAnalysis.controlFlow(CfgConfig.AllNodes(false), interp)
    val constants = ConstantAnalysis.constantInstructions(interp)

    val modInst = interp.initializeModule(mod)
    val res = interp.failure.fallible(
      interp.runMostGeneralClient(modInst, ConstantAnalysis.typedTop)
    )

    val allNodes = ControlFlow.allCfgNodes(List(modInst))
    val allInstructions = allNodes.filter(_.isInstruction)
    val deadInstructions = ControlFlow.deadInstruction(cfg, List(modInst))
    val allLabels = allNodes.filter(_.isInstanceOf[CfgNode.Labled])
    val deadLabels = ControlFlow.deadLabels(cfg)
    println(allInstructions)
    println(deadInstructions)
    println(deadLabels)
    // TODO This could probably be optimized
    println(mod)
    val instPositions = CfgRelatedMethods.getInstNodeLocation(deadInstructions)

    mod.copy(funcs = mod.funcs.zipWithIndex.map((f, f_idx) =>
      if (instPositions.keySet.contains(f_idx)) {
        Func(f.tpe, f.locals, f.body.indices.collect {
          case f_inst_idx if instPositions(f_idx).contains(f_inst_idx) => f.body(f_inst_idx)
        }.toVector)
      }
      else f
      ))