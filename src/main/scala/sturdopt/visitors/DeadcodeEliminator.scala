package sturdopt.visitors

import sturdopt.util.IfTarget.{AllAlive, EndLabelTarget, SingleInstructionTarget}
import sturdy.language.wasm.abstractions.CfgNode
import sturdopt.util.{FuncIdx, FuncIfTargetsMap, FuncInstrMap, FuncLabelMap, InstrIdx, LabelInst}
import swam.{LabelIdx, syntax}
import swam.syntax.{Unreachable, Block, Br, BrIf, BrTable, Call, Drop, Elem, Export, Func, GlobalGet, If, Inst, Loop, i32}

/**
  @param funcInstrLocs A Map representing the instructions to be removed
  @param deadLabelMap A Map representing the dead labels
  @return The module with removed deadcode
 */
class DeadcodeEliminator(funcInstrLocs: FuncInstrMap, deadLabelMap: FuncLabelMap, ifTargets: FuncIfTargetsMap) extends BaseModuleVisitor:

  val deadFunctions: Seq[FuncIdx] = funcInstrLocs.collect {
    // A function is dead if it's first instruction is dead
    case (funcIdx: FuncIdx, instrIndices: Seq[InstrIdx]) if instrIndices.contains(0) => funcIdx
  }.toSeq

  // indicates if the rest of the current block/loop/if-branch is dead and an unreachable has already been placed
  var blockIsDead: Boolean = false

  /**
    shifts funcIdx to account for the removal of deadFunctions
   */
  private def shiftFuncIdx(funcIdx: FuncIdx): FuncIdx = funcIdx - deadFunctions.count(_ < funcIdx)

  /**
   * Checks if Instruction at current funcPC in funcIdx is dead
   */
  private def instrIsDead(funcIdx: FuncIdx, shift: Int = 0): Boolean = funcInstrLocs.get(funcIdx) match
    case Some(instrIndices: Seq[InstrIdx]) if instrIndices.contains(funcPc+shift) => true
    case _ => false

  private def flattenLabelIdx(lbl: LabelIdx, lblDepth: Int, deadLblbDepths: Vector[Int]): LabelIdx =
    lbl - deadLblbDepths.count(_ >= lblDepth-lbl)
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

  /**
   * Visits the block body and resets blockIdDead since that global variable should only apply to the current block
   * @param block The body of a block, loop or if branch
   */
  private def visitBlockBody(blockBody: Vector[Inst], funcIdx: FuncIdx, lblDepth: Int, deadLblDepths: Vector[Int]): Vector[Inst] =
    val result = blockBody.flatMap(visitFuncInstr(_, funcIdx, lblDepth, deadLblDepths))
    blockIsDead = false
    result

  override def visitFunc(func: Func, funcIdx: Int): Seq[Func] =
    // remove dead functions in this step to keep the old funcIdx before removal intact as those are the ones referenced
    // in funcInstrLocs and deadLabelMap
    if deadFunctions.contains(funcIdx) then Seq()
    else
      funcPc = -1
      blockIsDead = false
      Seq(Func(func.tpe, func.locals.flatMap(visitFuncLocal(_, funcIdx)), func.body.flatMap(visitFuncInstr(_, funcIdx))))

  override def visitFuncInstr(funcInstr: Inst, funcIdx: FuncIdx, lblDepth: Int = 0, deadLblDepths: Vector[Int] = Vector.empty[Int]): Seq[Inst] =
    if blockIsDead then
      visitFuncInstrCounter(funcInstr)
      Seq.empty[Inst]
    else if instrIsDead(funcIdx, 1) then
      visitFuncInstrCounter(funcInstr)
      blockIsDead = true
      Seq(Unreachable) // Unreachables are needed here to preserve signatures of block, etc in some cases
    else
      funcPc += 1
      funcInstr match {
        // TODO Instead of using drop to recover the correct stack the instructions that pushed the value on the top of the stack should be removed
        case If(tpe, thenInstr, elseInstr) =>
          val ifTarget = try
            ifTargets(funcIdx)(funcPc)
          catch
            case e: NoSuchElementException => throw new NoSuchElementException(s"If instruction in line=${funcPc} is alive yet isn't contained in ifTargets Map!")
          if (ifTarget == AllAlive) then
            Seq(If(tpe,
              visitBlockBody(thenInstr, funcIdx, lblDepth + 1, deadLblDepths),
              visitBlockBody(elseInstr, funcIdx, lblDepth + 1, deadLblDepths)))
          // Edge case where Then and Else branches are empty from the beginning or the only reached one is empty
          else if (ifTarget == EndLabelTarget) then
            (thenInstr ++ elseInstr).foreach(visitFuncInstrCounter)
            Seq(Drop)
          // One of the branches is dead and ifTarget == InstructionTarget
          else
            val thenBranchDead: Boolean = (thenInstr.isEmpty || instrIsDead(funcIdx, 1))
            deadLabelMap.get(funcIdx) match
              // Label is dead
              case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.If).contains(funcPc)) =>
                val innerInstr =
                  if thenBranchDead then
                    thenInstr.foreach(visitFuncInstrCounter)
                    visitBlockBody(elseInstr, funcIdx, lblDepth + 1, deadLblDepths.appended(lblDepth + 1))
                  else
                    val theninst = visitBlockBody(thenInstr, funcIdx, lblDepth + 1, deadLblDepths.appended(lblDepth + 1))
                    elseInstr.foreach(visitFuncInstrCounter)
                    theninst
                Seq(Drop) ++ innerInstr
              case _ =>
                val innerInstr =
                  if thenBranchDead then
                    thenInstr.foreach(visitFuncInstrCounter)
                    visitBlockBody(elseInstr, funcIdx, lblDepth + 1, deadLblDepths)
                  else
                    val theninst = visitBlockBody(thenInstr, funcIdx, lblDepth + 1, deadLblDepths)
                    elseInstr.foreach(visitFuncInstrCounter)
                    theninst
                // If the if instruction is neither dead nor its label but one of the branches is empty, then it can be removed
                // but we need to replace it with a block since its label is references by some branch
                Seq(Drop) ++ Seq(Block(tpe, innerInstr))

        case Block(tpe, blockInstr) =>
          deadLabelMap.get(funcIdx) match
            case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.Block).contains(funcPc)) =>
              visitBlockBody(blockInstr, funcIdx, lblDepth + 1, deadLblDepths.appended(lblDepth + 1))
            case _ => Seq(Block(tpe, visitBlockBody(blockInstr, funcIdx, lblDepth + 1, deadLblDepths)))

        case Loop(tpe, loopInstr) =>
          deadLabelMap.get(funcIdx) match
            case Some(instrLocMap: Map[LabelInst, Seq[InstrIdx]]) if (instrLocMap(LabelInst.Loop).contains(funcPc)) =>
              visitBlockBody(loopInstr, funcIdx, lblDepth + 1, deadLblDepths.appended(lblDepth + 1))
            case _ => Seq(Loop(tpe, visitBlockBody(loopInstr, funcIdx, lblDepth + 1, deadLblDepths)))

        case Call(callFuncidx: FuncIdx) => Seq(Call(shiftFuncIdx(callFuncidx)))
        case Br(lbl: LabelIdx) => Seq(Br(flattenLabelIdx(lbl, lblDepth, deadLblDepths)))
        case BrIf(lbl: LabelIdx) =>
          // TODO for the case of EndLabelTarget determine whether the condition is always reached or never
          if (ifTargets(funcIdx)(funcPc) == SingleInstructionTarget || deadLblDepths.contains(lblDepth-lbl)) then
            Seq(Drop)
          else
            Seq(BrIf(flattenLabelIdx(lbl, lblDepth, deadLblDepths)))
        case BrTable(table: Vector[LabelIdx], lbl: LabelIdx) =>
          Seq(BrTable(table.map(flattenLabelIdx(_, lblDepth, deadLblDepths)), flattenLabelIdx(lbl, lblDepth, deadLblDepths)))

        case _ => Seq(funcInstr)
      }

  override def visitExport(exprt: Export): Seq[Export] = Seq(Export(exprt.fieldName, exprt.kind, shiftFuncIdx(exprt.index)))

  /**
   * Shift the elem function reference indices and use offsets to keep them in the same position in the table
   * while removing the dead references from the table.
   */
  override def visitElem(elem: Elem): Seq[Elem] =
    elem.init.zipWithIndex.foldLeft(Seq[Option[Elem]](None)) {
      case (acc: Seq[Option[Elem]], (funcIdx: FuncIdx, elemIdx: Int)) =>
        if deadFunctions.contains(funcIdx) then
          acc.appended(None)
        else
          acc.updated(acc.size-1, acc.last match
            case Some(lastElem: Elem) => Some(Elem(lastElem.table, lastElem.offset, lastElem.init.appended(shiftFuncIdx(funcIdx))))
            case None =>
              if elemIdx == 0 then
                Some(Elem(elem.table, elem.offset, Vector(shiftFuncIdx(funcIdx))))
              else
                // Per spec the offset can only be a single constant vector. Imported immutable globals are also allowed which could
                // cause an error here. This is okay for now since imports are not supported by the optimization yet
                val newOffset = elem.offset match
                  case Vector(i32.Const(value: Int)) => Vector(i32.Const(value+elemIdx))
                  case Vector(glob: GlobalGet) => throw new IllegalArgumentException("Imported global in elem not allowed for this optimization!")
                Some(Elem(elem.table, newOffset, Vector(shiftFuncIdx(funcIdx))))
          )
    }.flatten