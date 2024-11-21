package sturdopt.visitors

import sturdy.language.wasm.abstractions.CfgNode
import sturdopt.util.{FuncIdx, FuncInstrMap, FuncLabelMap, InstrIdx, LabelInst}
import swam.syntax
import swam.syntax.{Block, Call, Drop, Export, Func, If, Inst, Loop}

/**
  @param funcInstrLocs A Map representing the instructions to be removed
  @param deadLabelMap A Map representing the dead labels
  @return The module with removed deadcode
 */
class DeadcodeEliminator(funcInstrLocs: FuncInstrMap, deadLabelMap: FuncLabelMap) extends BaseModuleVisitor:

  val deadFunctions: Seq[FuncIdx] = funcInstrLocs.collect {
    // A function is dead if it's first instruction is dead
    case (funcIdx: FuncIdx, instrIndices: Seq[InstrIdx]) if instrIndices.contains(0) => funcIdx
  }.toSeq

  /**
    shifts funcIdx to account for the removal of deadFunctions
   */
  private def shiftFuncIdx(funcIdx: FuncIdx) = funcIdx - deadFunctions.count(_ < funcIdx)

  /**
   * Checks if Instruction at current funcPC in funcIdx is dead
   */
  private def instrIsDead(funcIdx: FuncIdx): Boolean = funcInstrLocs.get(funcIdx) match
    case Some(instrIndices: Seq[InstrIdx]) if instrIndices.contains(funcPc) => true
    case _ => false

  override def visitFunc(func: Func, funcIdx: Int): Seq[Func] =
    // remove dead functions in this step to keep the old funcIdx before removal intact as those are the ones referenced
    // in funcInstrLocs and deadLabelMap
    if deadFunctions.contains(funcIdx) then Seq()
    else
      funcPc = -1
      Seq(Func(func.tpe, func.locals.flatMap(visitFuncLocal(_, funcIdx)), func.body.flatMap(visitFuncInstr(_, funcIdx))))

  override def visitFuncInstr(funcInstr: Inst, funcIdx: FuncIdx): Seq[Inst] =
    funcPc += 1
    funcInstr match
      // TODO Instead of using drop to recover the correct stack the instructions that pushed the value on the top of the stack should be removed
      case If(tpe, thenInstr, elseInstr) => deadLabelMap.get(funcIdx) match
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.If).contains(funcPc) || instrIsDead(funcIdx)) =>
          Seq(Drop) ++ (thenInstr++elseInstr).flatMap(visitFuncInstr(_, funcIdx))
        case _ => Seq(If(tpe, thenInstr.flatMap(visitFuncInstr(_, funcIdx)), elseInstr.flatMap(visitFuncInstr(_, funcIdx))))
      case Block(tpe, blockInstr) => deadLabelMap.get(funcIdx) match
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.Block).contains(funcPc) || instrIsDead(funcIdx))  =>
          blockInstr.flatMap(visitFuncInstr(_, funcIdx))
        case _ => Seq(Block(tpe, blockInstr.flatMap(visitFuncInstr(_, funcIdx))))
      case Loop(tpe, loopInstr) => deadLabelMap.get(funcIdx) match
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.Loop).contains(funcPc) || instrIsDead(funcIdx)) =>
          loopInstr.flatMap(visitFuncInstr(_, funcIdx))
        case _ => Seq(Loop(tpe, loopInstr.flatMap(visitFuncInstr(_, funcIdx))))
      case Call(callFuncidx: FuncIdx) => Seq(Call(shiftFuncIdx(callFuncidx)))
      case _ =>
        if instrIsDead(funcIdx) then Seq.empty[Inst]
        else Seq(funcInstr)

  override def visitExport(exprt: Export): Seq[Export] = Seq(Export(exprt.fieldName, exprt.kind, shiftFuncIdx(exprt.index)))

  // TODO Just shifting the funcidx in elem is probably not enough. Test what happens if funcidx points to non existing function
  override def visitElemInit(funcidx: FuncIdx): Seq[FuncIdx] = Seq(shiftFuncIdx(funcidx))