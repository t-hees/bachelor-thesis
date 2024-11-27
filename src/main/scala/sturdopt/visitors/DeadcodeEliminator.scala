package sturdopt.visitors

import sturdy.language.wasm.abstractions.CfgNode
import sturdopt.util.{FuncIdx, FuncInstrMap, FuncLabelMap, InstrIdx, LabelInst}
import swam.{LabelIdx, syntax}
import swam.syntax.{Block, Br, BrIf, BrTable, Call, Drop, Export, Func, If, Inst, Loop}

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

  /**
   * Increases the funcPc counter accordingly without any other effects
   */
  private def visitFuncInstrCounter(funcInstr: Inst): Unit =
    funcPc += 1
    funcInstr match
      case If(tpe, thenInstr, elseInstr) =>
        thenInstr.foreach(visitFuncInstrCounter)
        elseInstr.foreach(visitFuncInstrCounter)
      case Block(tpe, blockInstr) => blockInstr.foreach(visitFuncInstrCounter)
      case Loop(tpe, loopInstr) => loopInstr.foreach(visitFuncInstrCounter)
      case _ => ()

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
        // If the if instruction is dead it is never reached and we don't need to add a drop
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrIsDead(funcIdx)) =>
          (thenInstr++elseInstr).foreach(visitFuncInstrCounter)
          Seq.empty[Inst]
        // If just the label of the if instruction is dead it is reached but defaults to one case. Here we need a drop
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.If).contains(funcPc)) =>
          Seq(Drop) ++ (thenInstr++elseInstr).flatMap(visitFuncInstr(_, funcIdx))
        case _ =>
          val optThenInstr = thenInstr.flatMap(visitFuncInstr(_, funcIdx))
          val optElseInstr = elseInstr.flatMap(visitFuncInstr(_, funcIdx))
          // If the if instruction is neither dead nor its label but one of the branches is empty, then it can be removed
          // but we need to replace it with a block since its label is references by some branch
          if (optThenInstr.isEmpty || optElseInstr.isEmpty) then Seq(Drop) ++ Seq(Block(tpe, optThenInstr++optElseInstr))
          else Seq(If(tpe, optThenInstr, optElseInstr))

      case Block(tpe, blockInstr) => deadLabelMap.get(funcIdx) match
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrIsDead(funcIdx))  =>
          blockInstr.foreach(visitFuncInstrCounter)
          Seq.empty[Inst]
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.Block).contains(funcPc))  =>
          blockInstr.flatMap(visitFuncInstr(_, funcIdx))
        case _ => Seq(Block(tpe, blockInstr.flatMap(visitFuncInstr(_, funcIdx))))

      case Loop(tpe, loopInstr) => deadLabelMap.get(funcIdx) match
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrIsDead(funcIdx)) =>
          loopInstr.foreach(visitFuncInstrCounter)
          Seq.empty[Inst]
        case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.Loop).contains(funcPc)) =>
          loopInstr.flatMap(visitFuncInstr(_, funcIdx))
        case _ => Seq(Loop(tpe, loopInstr.flatMap(visitFuncInstr(_, funcIdx))))

      case _ if instrIsDead(funcIdx) => Seq.empty[Inst]

      case Call(callFuncidx: FuncIdx) => Seq(Call(shiftFuncIdx(callFuncidx)))

      case _ => Seq(funcInstr)

  override def visitExport(exprt: Export): Seq[Export] = Seq(Export(exprt.fieldName, exprt.kind, shiftFuncIdx(exprt.index)))

  override def visitElemInit(funcidx: FuncIdx): Seq[FuncIdx] =
    // If the function is dead anyways then it doen't matter to which function this elem points to (so we choose 0)
    if deadFunctions.contains(funcidx) then Seq(0)
    else Seq(shiftFuncIdx(funcidx))