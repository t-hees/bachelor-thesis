package sturdopt.util

import sturdy.language.wasm.abstractions.CfgNode
import sturdy.language.wasm.generic.InstLoc.InFunction

object CfgRelatedMethods {

  /**
   * Gets the locations inside of Module function bodies from Instruction Nodes
   * in the form:
   * Map[FuncIdx, Seq[pc in function body]]
   */
  def getInstNodeLocation(nodes: Set[CfgNode]): Map[Int, Seq[Int]] = nodes.foldLeft(Map.empty[Int, Seq[Int]]) {
    case (acc, CfgNode.Instruction(inst, loc)) => loc match
      case InFunction(func, pc) =>
        val counters = acc.getOrElse(func.funcIx, Seq.empty[Int])
        acc.updated(func.funcIx, counters.appended(pc))
      case _ => ???
    case _ => throw IllegalArgumentException(s"$nodes should only contain Instructions!")
  }

}
