package sturdopt.util

import sturdy.language.wasm.abstractions.CfgNode
import sturdy.language.wasm.generic.InstLoc
import sturdy.language.wasm.generic.InstLoc.InFunction
import swam.syntax.Loop

type FuncIdx = Int
type InstrIdx = Int
type FuncInstrMap = Map[FuncIdx, Seq[InstrIdx]]
type FuncLabelMap = Map[FuncIdx, Map[LabelInst, Seq[InstrIdx]]]
type FuncIfTargetsMap = Map[FuncIdx, Map[InstrIdx, IfTarget]]

enum LabelInst:
  case Block, Loop, If

/*
For If instructions:
  AllAlive = Both then and else cases are reachable
  SingleInstructionTarget = Only one case is reachable (then or else)
  EndLabelTarget = All reachable cases are empty (always goes to end of entire if construct)
  LoopJumpTarget = Not possible
For BrIf instructions:
  AllAlive = Both jumping and not jumping are reachable
  SingleInstructionTarget = The condition is never reached (never jump)
  EndLabelTarget = Always goes to an EndLabel (unknown whether condition is always reached or never)
  LoopJumpTarget = Always Jump to Loop (condition always reached)
 */
enum IfTarget:
  case AllAlive, SingleInstructionTarget, EndLabelTarget, LoopJumpTarget

object CfgRelatedMethods {

  /**
   * Gets the locations inside of Module function bodies from Instruction Nodes
   * in the form:
   * Map[FuncIdx, Seq[pc in function body]]
   */
  def getInstNodeLocation(nodes: Set[CfgNode]): FuncInstrMap = nodes.foldLeft(Map.empty[FuncIdx, Seq[InstrIdx]]) {
    case (acc, cfgnode) =>
      getSingleInstLoc(cfgnode) match
        case InFunction(func, pc) =>
          val indices = acc.getOrElse(func.funcIx, Seq.empty[InstrIdx])
          acc.updated(func.funcIx, indices.appended(pc))
        case _ => ???
  }

  def getLabledInstLocation(nodes: Set[CfgNode.Labled]): FuncLabelMap = nodes.foldLeft(Map.empty[FuncIdx, Map[LabelInst, Seq[InstrIdx]]]) {
    case (acc, CfgNode.Labled(inst, loc)) =>
      val labelInst: LabelInst = inst match
        case block: swam.syntax.Block => LabelInst.Block
        case loop: swam.syntax.Loop => LabelInst.Loop
        case ifs: swam.syntax.If => LabelInst.If

      loc match
        case InFunction(func, pc) =>
          val funcMap = acc.getOrElse(func.funcIx, Map(LabelInst.Block -> Seq.empty[InstrIdx],
            LabelInst.Loop -> Seq.empty[InstrIdx],
            LabelInst.If -> Seq.empty[InstrIdx]))
          val indices = funcMap(labelInst)
          acc.updated(func.funcIx, funcMap.updated(labelInst, indices.appended(pc)))
        case _ => ???
  }

  /**
   * @param nodes Nodes of cfg (Must all be alive!)
   * @param edges Edges of cfg
   * @return Map indicating IfTargets for both If and BrIf instructions
   */
  def getIfTargets(nodes: Set[CfgNode], edges: Map[CfgNode, Seq[CfgNode]]): FuncIfTargetsMap =
    def getIfTargetsInnerFunction(acc: FuncIfTargetsMap, node: CfgNode, loc: InstLoc) = loc match
      case InFunction(func, pc) =>
        val targetType = edges(node) match
          // Somehow for Wasmbench file 83c55f871d03aafe4e021787d2a1701dd959c4eb51eb2f9bab0e312e2d9f9108.wasm
          // there is a brIF with 3 targets (2 of them being Block end labels in another function?) so we use >= 2 instead of == 2
          case targetNodes: Seq[CfgNode] if (targetNodes.size >= 2) => IfTarget.AllAlive
          case Seq(lblEnd: CfgNode.LabledEnd) => IfTarget.EndLabelTarget
          // LoopJumpTarget if the targeted loop appears before the brIf instruction
          case Seq(CfgNode.Labled(loop: Loop, loopLoc: InstLoc)) => loopLoc match
            case InFunction(_, loopPc) if loopPc < pc => IfTarget.LoopJumpTarget
            case _ => IfTarget.SingleInstructionTarget
          case Seq(inst: CfgNode) => IfTarget.SingleInstructionTarget
          case targetNodes: Seq[CfgNode] => throw new IllegalArgumentException(s"Target nodes ${targetNodes} from ${node} are of illegal type or node is dead")
        val innerMap = acc.getOrElse(func.funcIx, Map.empty[InstrIdx, IfTarget])
        acc.updated(func.funcIx, innerMap.updated(pc, targetType))
      case _ => ???

    nodes.foldLeft(Map.empty[FuncIdx, Map[InstrIdx, IfTarget]]) {
      case (acc, node) => node match
        case CfgNode.Labled(inst: swam.syntax.If, loc) => getIfTargetsInnerFunction(acc, node, loc)
        case CfgNode.Instruction(inst: swam.syntax.BrIf, loc) => getIfTargetsInnerFunction(acc, node, loc)
        case _ => acc
    }

  private def getSingleInstLoc(node: CfgNode): InstLoc = node match
    case CfgNode.Instruction(_, loc) => loc
    case CfgNode.Call(_, loc) => loc
    case CfgNode.Labled(_, loc) => loc
    case other: CfgNode => throw IllegalArgumentException(s"$other is not an instruction!")
    
}
