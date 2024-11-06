package sturdopt.util

import sturdy.language.wasm.abstractions.CfgNode
import sturdy.language.wasm.generic.InstLoc.InFunction

type FuncIdx = Int
type InstrIdx = Int
type FuncInstrMap = Map[FuncIdx, Seq[InstrIdx]]
type FuncLabelMap = Map[FuncIdx, Map[LabelInst, Seq[InstrIdx]]]

enum LabelInst:
  case Block, Loop, If

object CfgRelatedMethods {

  /**
   * Gets the locations inside of Module function bodies from Instruction Nodes
   * in the form:
   * Map[FuncIdx, Seq[pc in function body]]
   */
  def getInstNodeLocation(nodes: Set[CfgNode]): FuncInstrMap = nodes.foldLeft(Map.empty[FuncIdx, Seq[InstrIdx]]) {
    case (acc, CfgNode.Instruction(inst, loc)) => loc match
      case InFunction(func, pc) =>
        val indices = acc.getOrElse(func.funcIx, Seq.empty[InstrIdx])
        acc.updated(func.funcIx, indices.appended(pc))
      case _ => ???
    case _ => throw IllegalArgumentException(s"$nodes should only contain Instructions!")
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
}
