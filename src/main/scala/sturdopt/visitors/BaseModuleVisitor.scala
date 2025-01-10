package sturdopt.visitors

import sturdopt.util.{FuncIdx, InstrIdx}
import swam.{FuncType, LabelIdx, ValType}
import swam.syntax.{Br, BrIf, BrTable, Elem, Export, Func, Global, Import, Inst, Module}

class BaseModuleVisitor(mod: Module):
  
  var funcPc: Int = 0

  def visitModule() =
    val impFuncAmount = mod.imported.funcs.size
    Module(
      mod.types.flatMap(visitType),
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

  def visitType(tpe: FuncType): Seq[FuncType] = Seq(tpe)

  def visitFunc(func: Func, funcIdx: Int): Seq[Func] =
    funcPc = -1
    Seq(Func(func.tpe, func.locals.flatMap(visitFuncLocal(_, funcIdx)), func.body.flatMap(visitFuncInstr(_, funcIdx))))

  def visitFuncLocal(local: ValType, funcIdx: FuncIdx): Seq[ValType] = Seq(local)

  def visitFuncInstr(funcInstr: Inst, funcIdx: FuncIdx): Seq[Inst] =
    funcPc += 1
    Seq(funcInstr)

  def visitGlobal(global: Global): Seq[Global] = Seq(global)

  def visitImport(imprt: Import): Seq[Import] = Seq(imprt)

  def visitExport(exprt: Export): Seq[Export] = Seq(exprt)
  
  def visitElem(elem: Elem): Seq[Elem] = Seq(Elem(elem.table, elem.offset, elem.init.flatMap(visitElemInit)))
  
  def visitElemInit(funcidx: FuncIdx): Seq[FuncIdx] = Seq(funcidx)