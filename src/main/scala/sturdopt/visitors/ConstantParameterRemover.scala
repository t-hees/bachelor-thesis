package sturdopt.visitors

import sturdopt.util.*
import sturdy.language.wasm.Interpreter
import sturdy.language.wasm.analyses.ConstantAnalysis.{ConstantInstructionsLogger, Value}
import sturdy.language.wasm.generic.InstLoc.InFunction
import swam.syntax.*
import swam.{FuncType, LabelIdx, TypeIdx, ValType, syntax}

class ConstantParameterRemover(mod: Module, remParams: Map[FuncIdx, Set[Int]]) extends BaseModuleVisitor(mod):

  private var newTypes: Vector[FuncType] = mod.types
  private val funcTypeMapping: scala.collection.mutable.Map[FuncIdx, TypeIdx] = scala.collection.mutable.Map.empty[FuncIdx, TypeIdx]
  private val callDrops: Map[FuncIdx, Int] = remParams.filter((funcIdx, _) => 
    (funcIdx >= mod.imported.funcs.size) && !mod.elem.flatMap {
        case Elem(_, _, init) => init
      }.contains(funcIdx) // Only non imported functions that aren't referenced in any table (indirect call parameters removal not supported)
    ).map { (funcIdx: FuncIdx, remParamIndices: Set[Int]) =>
      val funcType: FuncType = mod.types(mod.funcs(funcIdx - mod.imported.funcs.size).tpe)
      val newFuncType: FuncType = FuncType(
        funcType.params.zipWithIndex.reverse.dropWhile((_, pIdx) => remParamIndices.contains(pIdx)).reverse.map(_._1),
        funcType.t
      )
      funcTypeMapping(funcIdx) = newTypes.indexOf(newFuncType) match
        case -1 => // case when the new required type with removed parameters doesn't exist yet
          newTypes = newTypes.appended(newFuncType)
          newTypes.size - 1
        case typeIdx: TypeIdx => typeIdx
      (funcIdx, funcType.params.size - newFuncType.params.size)
    }

  override def visitModule(): Module =
    val impFuncAmount = mod.imported.funcs.size
    Module(
      newTypes,
      // The index gets shifted by the amount of imported functions since those always come before!
      mod.funcs.zipWithIndex.flatMap((func, funcIdx) => visitFunc(func, funcIdx+impFuncAmount)),
      mod.tables,
      mod.mems,
      mod.globals.flatMap(visitGlobal),
      mod.elem.flatMap(visitElem),
      mod.data,
      mod.start,
      mod.imports.flatMap(visitImport),
      mod.exports.flatMap(visitExport)
    )

  override def visitFunc(func: Func, funcIdx: Int): Seq[Func] =
    funcPc = -1
    Seq(Func(funcTypeMapping.getOrElse(funcIdx, func.tpe), // set the new function types
      func.locals.flatMap(visitFuncLocal(_, funcIdx)),
      func.body.flatMap(visitFuncInstr(_, funcIdx))))

  override def visitFuncInstr(funcInstr: Inst, funcIdx: FuncIdx): Seq[Inst] =
    funcPc += 1
    funcInstr match
      case If(tpe, thenInstr, elseInstr) => Seq(
        If(tpe, thenInstr.flatMap(visitFuncInstr(_, funcIdx)), elseInstr.flatMap(visitFuncInstr(_, funcIdx)))
      )
      case Block(tpe, blockInstr) => Seq(Block(tpe, blockInstr.flatMap(visitFuncInstr(_, funcIdx))))
      case Loop(tpe, loopInstr) => Seq(Loop(tpe, loopInstr.flatMap(visitFuncInstr(_, funcIdx))))

      case Call(calledIdx: FuncIdx) => Seq.fill(callDrops.getOrElse(calledIdx, 0))(Drop) ++ Seq(Call(calledIdx))
      case _ => Seq(funcInstr)
