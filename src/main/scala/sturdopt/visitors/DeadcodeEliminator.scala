package sturdopt.visitors

import sturdy.language.wasm.abstractions.CfgNode
import sturdopt.util.{FuncIdx, LabelInst, FuncInstrMap, FuncLabelMap, InstrIdx}
import swam.syntax
import swam.syntax.{Drop, If, Inst}

/*
  funcInstrLocs are the instructions to be removed
 */
class DeadcodeEliminator(funcInstrLocs: FuncInstrMap, deadLabelMap: FuncLabelMap) extends BaseModuleVisitor:

  override def visitFuncInstr(funcInstr: Inst, funcIdx: FuncIdx): Seq[Inst] =
    funcPc += 1
    funcInstr match
      // TODO Instead of using drop to recover the correct stack the instructions that pushed the value on the top of the stack should be removed
      case If(tpe, thenInstr, elseInstr) => deadLabelMap.get(funcIdx) match
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if instrLocMap(LabelInst.If).contains(funcPc) =>
          Seq(Drop) ++ (thenInstr++elseInstr).flatMap(visitFuncInstr(_, funcIdx))
        case _ => Seq(If(tpe, thenInstr.flatMap(visitFuncInstr(_, funcIdx)), elseInstr.flatMap(visitFuncInstr(_, funcIdx))))
      case _ => funcInstrLocs.get(funcIdx) match
        case Some(instrIndices: Seq[InstrIdx]) if instrIndices.contains(funcPc) => Seq.empty[Inst]
        case _ => Seq(funcInstr)