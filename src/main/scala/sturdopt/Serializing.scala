package sturdopt

import swam.syntax.{Func, FuncBody, LocalEntry, Module, Section}
import swam.binary.ModuleStream
import swam.binary.WasmCodec
import scodec.bits.ByteVector

object Serializing:
  def serialize(module: Module): ByteVector =
    var result: ByteVector = ModuleStream.header
    val sections: Vector[Option[Section]] = Vector(
      Some(Section.Types(module.types)),
      Some(Section.Imports(module.imports)),
      Some(Section.Functions(module.funcs.map(func => func.tpe))),
      Some(Section.Tables(module.tables)),
      Some(Section.Memories(module.mems)),
      Some(Section.Globals(module.globals)),
      Some(Section.Exports(module.exports)),
      module.start match
        case Some(funcidx) => Some(Section.Start(funcidx))
        case None => None,
      Some(Section.Elements(module.elem)),
      // datacount section might be necessary here
      Some(Section.Code(module.funcs.map {
        case Func(_, locals, body) =>
          val locs = locals.foldLeft(Vector.empty[LocalEntry]) {
            case (acc :+ LocalEntry(count, tpe), curr_tpe) if curr_tpe == tpe =>
              acc :+ LocalEntry(count + 1, tpe)
            case (acc, curr_tpe) =>
              acc :+ LocalEntry(1, curr_tpe)
          }
          FuncBody(locs, body)
      })),
      Some(Section.Datas(module.data))
      // custom section ignored since it is not parsed in the swam parser
    )

    sections.foldLeft(ModuleStream.header.toBitVector)((section1, section2) => section2 match
      case Some(sect2) =>
        section1 ++ WasmCodec.section.encode(sect2).require
      case None =>
        section1
    ).toByteVector