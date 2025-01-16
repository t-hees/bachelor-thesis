package sturdopt.visitors

import sturdopt.util.*
import sturdy.language.wasm.Interpreter
import sturdy.language.wasm.analyses.ConstantAnalysis.{ConstantInstructionsLogger, Value}
import sturdy.language.wasm.generic.InstLoc.InFunction
import swam.syntax.*
import swam.{LabelIdx, syntax}

/**
 * Just removes every undesired non block/loop/if instruction from module without any other effects
 * @param mod The module
 * @param removableInst The instructions that should be removed from module
 * @param neededDrops The instructions after which a specified amount of drops have to be added
 */
class DropsRemover(mod: Module, removableInst: FuncInstrMap, neededDrops: Map[FuncIdx, Seq[(InstrIdx, Int)]]) extends BaseModuleVisitor(mod):

  override def visitFuncInstr(funcInstr: Inst, funcIdx: FuncIdx): Seq[Inst] =
    funcPc += 1
    val newAppendedDrops = appendedDrops(funcIdx)
    funcInstr match
      case If(tpe, thenInstr, elseInstr) => Seq(
        If(tpe, thenInstr.flatMap(visitFuncInstr(_, funcIdx)), elseInstr.flatMap(visitFuncInstr(_, funcIdx)))
      ) ++ newAppendedDrops
      case Block(tpe, blockInstr) => Seq(Block(tpe, blockInstr.flatMap(visitFuncInstr(_, funcIdx)))) ++ newAppendedDrops
      case Loop(tpe, loopInstr) => Seq(Loop(tpe, loopInstr.flatMap(visitFuncInstr(_, funcIdx)))) ++ newAppendedDrops

      case inst: Inst => removableInst.get(funcIdx) match
        case Some(instrIndices: Seq[InstrIdx]) if instrIndices.contains(funcPc) => Seq()
        case _ => Seq(inst) ++ newAppendedDrops

  private def appendedDrops(funcIdx: FuncIdx): Seq[Inst] = neededDrops.get(funcIdx) match
    case Some(drops: Seq[(InstrIdx, Int)]) => drops.find((dropLoc, _) => dropLoc == funcPc) match
      case Some((_, dropsAmount: Int)) => Seq.fill(dropsAmount)(Drop)
      case _ => Seq.empty[Inst]
    case _ => Seq.empty[Inst]