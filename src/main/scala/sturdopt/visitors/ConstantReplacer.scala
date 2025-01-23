package sturdopt.visitors

import sturdopt.util.*
import sturdy.language.wasm.analyses.ConstantAnalysis.Value
import swam.syntax.*
import swam.{LabelIdx, ValType, syntax}
import sturdy.language.wasm.Interpreter
import sturdy.language.wasm.analyses.ConstantAnalysis.ConstantInstructionsLogger
import sturdy.language.wasm.generic.InstLoc.InFunction

class ConstantReplacer(mod: Module, constants: Map[FuncIdx, Map[InstrIdx, Value]]) extends BaseModuleVisitor(mod):

  // noGetParams: Indices of params in a function of which the local.gets have been declared as constant
  // setParams: Indices of params in a function which are used by a set/tee. Those can not be removed!
  private val noGetParams, setParams: Map[FuncIdx, scala.collection.mutable.Set[Int]] =
    (0 until mod.funcs.size+mod.imported.funcs.size).map(i => i -> scala.collection.mutable.Set.empty[Int]).toMap

  private def getConstantValue(funcIdx: FuncIdx): Option[Inst] = constants.get(funcIdx) match
    case Some(innerMap: Map[InstrIdx, Value]) => innerMap.get(funcPc) match
      case Some(value: Value) => value match
        case Value.Int32(i) => Some(i32.Const(i.get))
        case Value.Int64(i) => Some(i64.Const(i.get))
        case Value.Float32(i) => Some(f32.Const(i.get))
        case Value.Float64(i) => Some(f64.Const(i.get))
        case _ => None
      case None => None
    case None => None

  private def getFuncParams(funcIdx: FuncIdx): Vector[ValType] =
    if funcIdx < mod.imported.funcs.size then mod.imported.funcs(funcIdx).params
    else mod.types(mod.funcs(funcIdx - mod.imported.funcs.size).tpe).params

  override def visitModule(): Module =
    val newMod = super.visitModule()
    ConstantParameterRemover(newMod,
      noGetParams.map((fIdx, params) => fIdx -> params.diff(setParams(fIdx)).toSet)
    ).visitModule()

  override def visitFuncInstr(funcInstr: Inst, funcIdx: FuncIdx): Seq[Inst] =
    funcPc += 1
    funcInstr match
      case If(tpe, thenInstr, elseInstr) => Seq(
        If(tpe, thenInstr.flatMap(visitFuncInstr(_, funcIdx)), elseInstr.flatMap(visitFuncInstr(_, funcIdx)))
      )
      case Block(tpe, blockInstr) => Seq(Block(tpe, blockInstr.flatMap(visitFuncInstr(_, funcIdx))))
      case Loop(tpe, loopInstr) => Seq(Loop(tpe, loopInstr.flatMap(visitFuncInstr(_, funcIdx))))

      case unop: (Unop | Testop | LoadInst) => getConstantValue(funcIdx) match
        case Some(const: Inst) => Seq(Drop, const)
        case None => Seq(unop)

      case binop: (Binop | Relop) => getConstantValue(funcIdx) match
        case Some(const: Inst) => Seq(Drop, Drop, const)
        case None => Seq(binop)

      case LocalGet(localIdx) => getConstantValue(funcIdx) match
        case Some(const: Inst) =>
          if (localIdx < getFuncParams(funcIdx).size) then noGetParams(funcIdx) += localIdx
          Seq(const)
        case None => Seq(LocalGet(localIdx))

      case LocalSet(localIdx) if (localIdx < getFuncParams(funcIdx).size) =>
        setParams(funcIdx) += localIdx
        Seq(LocalSet(localIdx))
      case LocalTee(localIdx) if (localIdx < getFuncParams(funcIdx).size) =>
        setParams(funcIdx) += localIdx
        Seq(LocalTee(localIdx))

      case getInst: GlobalGet => getConstantValue(funcIdx) match
        case Some(const: Inst) => Seq(const)
        case None => Seq(getInst)

      case selectInst: Select.type => getConstantValue(funcIdx) match
        case Some(const: Inst) => Seq(Drop, Drop, Drop, const)
        case None => Seq(selectInst)

      // Note: Sturdy currently doesn't seem to report constant calls
      case call: (Call | CallIndirect) => getConstantValue(funcIdx) match
        case Some(const: Inst) =>
          val paramSize: Int = call match
            case Call(callFuncIdx) => getFuncParams(callFuncIdx).size
            case CallIndirect(typeidx) => mod.types(typeidx).params.size + 1 // +1 because of the table index
          Seq.fill(paramSize)(Drop) ++ Seq(const)
        case None => Seq(call)

      case _ => Seq(funcInstr)
