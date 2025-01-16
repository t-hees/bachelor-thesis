package sturdopt.optimizations

import sturdopt.util.{CfgRelatedMethods, FuncIdx, FuncInstrMap, InstrIdx}
import sturdopt.visitors.DropsRemover
import sturdy.fix
import sturdy.fix.StackConfig
import sturdy.language.wasm.abstractions.{CfgConfig, CfgNode, ControlFlow}
import sturdy.language.wasm.analyses.{ConstantAnalysis, FixpointConfig, Insensitive, WasmConfig}
import sturdy.language.wasm.generic.InstLoc.InFunction
import sturdy.language.wasm.generic.{FrameData, FunctionInstance}
import swam.ModuleLoader
import swam.binary.ModuleParser
import swam.syntax.{AConst, Binop, Convertop, Drop, Func, GlobalGet, Inst, LoadInst, LoadNInst, LocalGet, Module, Nop, Relop, Select, Testop, Unop}

object DropsOptimization:

  def eliminateDrops(mod: Module): Module =
    val stackConfig = StackConfig.StackedStates()
    val interp = new ConstantAnalysis.Instance(FrameData.empty, Iterable.empty, WasmConfig(ctx = Insensitive, fix = FixpointConfig(fix.iter.Config.Innermost(StackConfig.StackedStates()))))
    val cfg = ConstantAnalysis.controlFlow(CfgConfig.AllNodes(false), interp)

    val modInst = interp.initializeModule(mod)
    val res = interp.failure.fallible(
      interp.runMostGeneralClient(modInst, ConstantAnalysis.typedTop)
    )

    val reverseEdges = cfg.getReverseEdges.map {
      case (from, to) => (from.node, to.keys.map(_.node).toSeq)
    }

    val dropsAnalysis = new DropsAnalysis(reverseEdges)
    val (removableInstructions, neededDrops) = dropsAnalysis.analyseDrops()

    DropsRemover(mod, removableInstructions, neededDrops).visitModule()

  
  private class DropsAnalysis(revEdges: Map[CfgNode, Seq[CfgNode]]):
    private class StackChangeUnsupported(msg: String) extends Exception(msg)
    // Indicates if stack element optional or mandatory. Optional ones can be removed
    enum StackType: 
      case Opt, Mnd
      
    // Location of removable Instructions
    //var remInst = Map.empty[FuncIdx, Seq[InstrIdx]]
    val remInst = scala.collection.mutable.Set.empty[CfgNode]
    // Location of Instructions before which as many drops have to be added as indicated by the Int
    //var neededDrops = Map.empty[FuncIdx, Seq[(InstrIdx, Int)]]
    val neededDrops = scala.collection.mutable.Map.empty[CfgNode, Int]

    def analyseDrops(): (FuncInstrMap, Map[FuncIdx, Seq[(InstrIdx, Int)]]) =
      val drops = revEdges.keys.filter(_ match
        case CfgNode.Instruction(Drop ,_) => true
        case _ => false
      )
      drops.foreach( d => if !remInst.contains(d) then processBackwards(d) )
      remInst.foreach(_)
      (CfgRelatedMethods.getInstNodeLocation(remInst.toSet), CfgRelatedMethods.getNeededDrops(neededDrops.toMap))

    def processBackwards(node: CfgNode, stack: Seq[StackType] = List.empty[StackType]): Unit =
      try {
        val (consumed, produced, stackType) = stackChange(node)
        val newStack =
          if stackType == StackType.Opt then
            if produced == 0 then
              List.fill(consumed)(StackType.Opt) ++ stack
            else // produced == 1
              List.fill(consumed)(stack.head) ++ stack.tail
          else ???

        remInst += node
        if newStack.nonEmpty then revEdges(node).foreach(processBackwards(_, newStack))
      } catch {
        case e: StackChangeUnsupported =>
          neededDrops(node) = stack.size
      }

    /**
     * Changes on the stack performed by the instruction of node
     * @return (consumed amount, produced amount, StackType)
     */
    def stackChange(node: CfgNode): (Int, Int, StackType) = node match
      case CfgNode.Instruction(inst: Inst, _) => inst match
        // while nop technically doesn't consume or produce anything it doesn't matter for the implementation
        case _: (Unop | Testop | Convertop) | Nop => (1, 1, StackType.Opt)
        case _: (Binop | Relop) => (2, 1, StackType.Opt)
        case _: (AConst | LoadInst | LoadNInst) | LocalGet(_) | GlobalGet(_) => (0, 1, StackType.Opt)
        // TODO Implement store (with StackType.Mnd)
        case Select => (3, 1, StackType.Opt)
        case Drop => (1, 0, StackType.Opt)
        case _ => throw new StackChangeUnsupported(s"Instruction type of $node not supported")
      case _ => throw new StackChangeUnsupported(s"Instruction type of $node not supported")
