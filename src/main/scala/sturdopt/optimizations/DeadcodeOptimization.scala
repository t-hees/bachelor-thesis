package sturdopt.optimizations

import sturdopt.util.CfgRelatedMethods
import sturdopt.visitors.DeadcodeEliminator
import sturdy.fix
import swam.ModuleLoader
import swam.binary.ModuleParser
import swam.syntax.{Func, Module}
import sturdy.fix.StackConfig
import sturdy.language.wasm.abstractions.{CfgConfig, CfgNode, ControlFlow}
import sturdy.language.wasm.analyses.{ConstantAnalysis, FixpointConfig, Insensitive, WasmConfig}
import sturdy.language.wasm.generic.FrameData
import sturdy.language.wasm.generic.InstLoc.InFunction

object DeadcodeOptimization:

  def eliminateDeadcode(mod: Module): Module =
    val stackConfig = StackConfig.StackedStates()
    val interp = new ConstantAnalysis.Instance(FrameData.empty, Iterable.empty, WasmConfig(ctx = Insensitive, fix = FixpointConfig(fix.iter.Config.Innermost(StackConfig.StackedStates()))))
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
    println(s"Dead instructions: ${deadInstructions}")
    println(s"Dead labels: ${deadLabels}")
    val deadFuncInstrMap = CfgRelatedMethods.getInstNodeLocation(deadInstructions)
    val deadLabelMap = CfgRelatedMethods.getLabledInstLocation(deadLabels)
    
    DeadcodeEliminator(deadFuncInstrMap, deadLabelMap).visitModule(mod)