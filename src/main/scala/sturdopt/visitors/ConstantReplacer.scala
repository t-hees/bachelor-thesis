package sturdopt.visitors

import sturdopt.util.*
import sturdy.language.wasm.analyses.ConstantAnalysis.Value
import swam.syntax.*
import swam.{FuncType, LabelIdx, TypeIdx, ValType, syntax}
import sturdy.language.wasm.Interpreter
import sturdy.language.wasm.analyses.ConstantAnalysis.ConstantInstructionsLogger
import sturdy.language.wasm.generic.InstLoc.InFunction

class ConstantReplacer(mod: Module, constants: Map[FuncIdx, Map[InstrIdx, Value]]) extends BaseModuleVisitor(mod):

  /**
   * A global variable that holds the total amount of added drops during the entire optimization
   */
  var totalAddedDrops: Int = 0
  
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
        case Some(const: Inst) => 
          totalAddedDrops += 1
          Seq(Drop, const)
        case None => Seq(unop)

      case binop: (Binop | Relop) => getConstantValue(funcIdx) match
        case Some(const: Inst) => 
          totalAddedDrops += 2
          Seq(Drop, Drop, const)
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
        case Some(const: Inst) => 
          totalAddedDrops += 3
          Seq(Drop, Drop, Drop, const)
        case None => Seq(selectInst)

      case call: (Call | CallIndirect) => getConstantValue(funcIdx) match
        case Some(const: Inst) =>
          val paramSize: Int = call match
            case Call(callFuncIdx) => getFuncParams(callFuncIdx).size
            case CallIndirect(typeidx) => mod.types(typeidx).params.size + 1 // +1 because of the table index
          totalAddedDrops += paramSize  
          Seq.fill(paramSize)(Drop) ++ Seq(const)
        case None => Seq(call)

      case _ => Seq(funcInstr)

  /**
   * This subclass is intended to be used by the ConstantReplacer class to remove constant parameters
   * @param constParams The integers of parameters of every function that is declared constant. These values are supposed to
   *                  come from the constant instruction removal and still need to be filtered
   */
  private class ConstantParameterRemover(mod: Module, constParams: Map[FuncIdx, Set[Int]]) extends BaseModuleVisitor(mod):

    private val updatedTypes: scala.collection.mutable.ArrayBuffer[FuncType] = mod.types.to(scala.collection.mutable.ArrayBuffer)
    private val remParams: scala.collection.mutable.Map[FuncIdx, Set[Int]] = scala.collection.mutable.Map.empty[FuncIdx ,Set[Int]]
    private val funcTypeMapping: scala.collection.mutable.Map[FuncIdx, TypeIdx] = scala.collection.mutable.Map.empty[FuncIdx, TypeIdx]
    private val callDrops: scala.collection.mutable.Map[FuncIdx, Int] = scala.collection.mutable.Map.empty[FuncIdx, Int]
    
    constParams.filterNot((funcIdx: FuncIdx, paramIndices: Set[Int]) =>
      paramIndices.isEmpty ||
      // Only non imported functions that aren't referenced in any table (indirect call parameters removal not supported)
      (funcIdx < mod.imported.funcs.size) || mod.elem.exists{ case Elem(_, _, init) => init.contains(funcIdx)}
    ).foreach((funcIdx: FuncIdx, paramIndices: Set[Int]) =>
      val oldFuncType: FuncType = mod.types(mod.funcs(funcIdx - mod.imported.funcs.size).tpe)
      // Only remove constant parameters sequentially from the back until one is not constant
      val actualRemParams = (oldFuncType.params.size-1).to(0, -1).takeWhile(paramIndices.contains).toSet
      val newFuncType: FuncType = FuncType(
        oldFuncType.params.zipWithIndex.filterNot((_, idx) => actualRemParams.contains(idx)).map(_._1),
        oldFuncType.t
      )
      remParams(funcIdx) = actualRemParams
      funcTypeMapping(funcIdx) = updatedTypes.indexOf(newFuncType) match
        case -1 => // case when the new required type with removed parameters doesn't exist yet
          updatedTypes += newFuncType
          updatedTypes.size - 1
        case typeIdx: TypeIdx => typeIdx
      callDrops(funcIdx) = oldFuncType.params.size - newFuncType.params.size
    )

    override def visitModule(): Module =
      val impFuncAmount = mod.imported.funcs.size
      Module(
        updatedTypes.toVector,
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

        case Call(calledIdx: FuncIdx) => 
          val dropAmnt = callDrops.getOrElse(calledIdx, 0)
          totalAddedDrops += dropAmnt
          Seq.fill(dropAmnt)(Drop) ++ Seq(Call(calledIdx))

        case LocalGet(localIdx) =>
          // Remove local instructions that reference a removed parameter. Assuming that the constant instruction
          // removal worked as expected this should only apply to unreachable local instructions
          if remParams.getOrElse(funcIdx, Set.empty).contains(localIdx) then Seq(Unreachable)
          // Otherwise shift the referenced localIdx by the removed parameters
          else Seq(LocalGet(localIdx - remParams.getOrElse(funcIdx, Set.empty).count(_ < localIdx)))
        case LocalSet(localIdx) => // same as localGet
          if remParams.getOrElse(funcIdx, Set.empty).contains(localIdx) then Seq(Unreachable)
          else Seq(LocalSet(localIdx - remParams.getOrElse(funcIdx, Set.empty).count(_ < localIdx)))
        case LocalTee(localIdx) => // same as localGet
          if remParams.getOrElse(funcIdx, Set.empty).contains(localIdx) then Seq(Unreachable)
          else Seq(LocalTee(localIdx - remParams.getOrElse(funcIdx, Set.empty).count(_ < localIdx)))

        case _ => Seq(funcInstr)
