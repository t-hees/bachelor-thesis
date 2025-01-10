package sturdopt.visitors

import sturdopt.util.*
import sturdy.language.wasm.analyses.ConstantAnalysis.Value
import swam.syntax.*
import swam.{LabelIdx, syntax}
import sturdy.language.wasm.Interpreter
import sturdy.language.wasm.analyses.ConstantAnalysis.ConstantInstructionsLogger
import sturdy.language.wasm.generic.InstLoc.InFunction

class ConstantReplacer(mod: Module, constants: Map[FuncIdx, Map[InstrIdx, Value]]) extends BaseModuleVisitor(mod):

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

      case getInst: (LocalGet | GlobalGet) => getConstantValue(funcIdx) match
        case Some(const: Inst) => Seq(const)
        case None => Seq(getInst)

      // TODO Sturdy always wrongly reports last constant on stack as return value. See broken_examples in resources
      case selectInst: Select.type => getConstantValue(funcIdx) match
        case Some(const: Inst) => Seq(Drop, Drop, Drop, const)
        case None => Seq(selectInst)

      // Note: Sturdy currently doesn't seem to report constant calls
      case call: (Call | CallIndirect) => getConstantValue(funcIdx) match
        case Some(const: Inst) =>
          val paramSize: Int = call match
            case Call(funcidx) => 
              if funcidx < mod.imported.funcs.size then mod.imported.funcs(funcidx).params.size
              else mod.types(mod.funcs(funcidx-mod.imported.funcs.size).tpe).params.size
            case CallIndirect(typeidx) => mod.types(typeidx).params.size
          Seq.fill(paramSize)(Drop) ++ Seq(const)
        case None => Seq(call)

      case _ => Seq(funcInstr)
