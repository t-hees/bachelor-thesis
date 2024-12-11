package sturdopt.util

import sturdy.language.wasm.abstractions.CfgNode
import sturdy.language.wasm.generic.InstLoc
import sturdy.language.wasm.generic.InstLoc.InFunction

type FuncIdx = Int
type InstrIdx = Int
type FuncInstrMap = Map[FuncIdx, Seq[InstrIdx]]
type FuncLabelMap = Map[FuncIdx, Map[LabelInst, Seq[InstrIdx]]]
type FuncIfTargetsMap = Map[FuncIdx, Map[InstrIdx, IfTarget]]

enum LabelInst:
  case Block, Loop, If

enum IfTarget:
  case AllAlive, ThenDead, ElseDead, AllEmpty

object CfgRelatedMethods {

  /**
   * Gets the locations inside of Module function bodies from Instruction Nodes
   * in the form:
   * Map[FuncIdx, Seq[pc in function body]]
   */
  def getInstNodeLocation(nodes: Set[CfgNode]): FuncInstrMap = nodes.foldLeft(Map.empty[FuncIdx, Seq[InstrIdx]]) {
    case (acc, cfgnode) =>
      getSingleInstLoc(cfgnode) match
        // TODO Implement with InInit which would be the _start function
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
        // TODO Implement with InInit which would be the _start function
        case InFunction(func, pc) =>
          val funcMap = acc.getOrElse(func.funcIx, Map(LabelInst.Block -> Seq.empty[InstrIdx],
            LabelInst.Loop -> Seq.empty[InstrIdx],
            LabelInst.If -> Seq.empty[InstrIdx]))
          val indices = funcMap(labelInst)
          acc.updated(func.funcIx, funcMap.updated(labelInst, indices.appended(pc)))
        case _ => ???
  }

  def getIfTargets(nodes: Set[CfgNode], edges: Map[CfgNode, Seq[CfgNode]]): FuncIfTargetsMap = nodes.foldLeft(Map.empty[FuncIdx, Map[InstrIdx, IfTarget]]) {
    case (acc, node) => node match
      case CfgNode.Labled(inst: swam.syntax.If, loc) => loc match
        case InFunction(func, pc) =>
          val targetType = edges(node) match
            case nodes: Seq[CfgNode] if (nodes.size == 2) => IfTarget.AllAlive
            case Seq(inst: CfgNode) if inst.isInstruction => getSingleInstLoc(inst) match
              case InFunction(_, targetPc) if (targetPc == pc+1) => IfTarget.ElseDead
              case _ => IfTarget.ThenDead
            case Seq(inst: CfgNode.LabledEnd) => IfTarget.AllEmpty
            case _ => throw new IllegalArgumentException(s"Edges from ${node} are of illegal type or node is dead")

          val innerMap = acc.getOrElse(func.funcIx, Map.empty[InstrIdx, IfTarget])
          acc.updated(func.funcIx, innerMap.updated(pc, targetType))
      case _ => acc
  }

  private def getSingleInstLoc(node: CfgNode): InstLoc = node match
    case CfgNode.Instruction(_, loc) => loc
    case CfgNode.Call(_, loc) => loc
    case CfgNode.Labled(_, loc) => loc
    case other: CfgNode => throw IllegalArgumentException(s"$other is not an instruction!")
}
