package sturdopt.optimizations

import sturdopt.util.CfgRelatedMethods.getConstantsMap
import sturdopt.visitors.ConstantReplacer
import sturdy.fix
import sturdy.fix.StackConfig
import sturdy.language.wasm.abstractions.{CfgConfig, CfgNode, ControlFlow}
import sturdy.language.wasm.analyses.{ConstantAnalysis, FixpointConfig, Insensitive, WasmConfig}
import sturdy.language.wasm.generic.InstLoc.InFunction
import sturdy.language.wasm.generic.{FrameData, FunctionInstance}
import swam.ModuleLoader
import swam.binary.ModuleParser
import swam.syntax.{Func, Module}

object ConstantsOptimization:

  def replaceConstants(mod: Module): Module =
    val stackConfig = StackConfig.StackedStates()
    val interp = new ConstantAnalysis.Instance(FrameData.empty, Iterable.empty, WasmConfig(ctx = Insensitive, fix = FixpointConfig(fix.iter.Config.Innermost(StackConfig.StackedStates()))))
    val cfg = ConstantAnalysis.controlFlow(CfgConfig.AllNodes(false), interp)
    val constants = ConstantAnalysis.constantInstructions(interp)

    val modInst = interp.initializeModule(mod)
    val res = interp.failure.fallible(
      interp.runMostGeneralClient(modInst, ConstantAnalysis.typedTop)
    )
    val constantsGet = constants.get
    println(s"Constants: ${constantsGet}")
    ConstantReplacer(mod, getConstantsMap(constantsGet)).visitModule()