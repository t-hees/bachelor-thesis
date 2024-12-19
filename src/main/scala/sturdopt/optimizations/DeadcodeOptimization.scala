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
import sturdy.language.wasm.generic.{FrameData, FunctionInstance}
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

    val deadInstructions = ControlFlow.deadInstruction(cfg, List(modInst))
    val aliveInstructions = ControlFlow.allCfgNodes(List(modInst)).filter(_.isInstruction).diff(deadInstructions)
    val deadLabels = ControlFlow.deadLabels(cfg)
    println(s"Dead instructions: ${deadInstructions}")
    println(s"Dead labels: ${deadLabels}")
    
    val allEdges = cfg.getEdges.map {
      case (from, to) => (from.node, to.keys.map(_.node).toSeq)
    }
    val ifTargets = CfgRelatedMethods.getIfTargets(aliveInstructions, allEdges)
    val deadFuncInstrMap = CfgRelatedMethods.getInstNodeLocation(deadInstructions)
    val deadLabelMap = CfgRelatedMethods.getLabledInstLocation(deadLabels)
    
    DeadcodeEliminator(deadFuncInstrMap, deadLabelMap, ifTargets).visitModule(mod)