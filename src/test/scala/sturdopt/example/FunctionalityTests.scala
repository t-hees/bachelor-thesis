package sturdopt.example

import sturdopt.Parsing
import sturdopt.Serializing
import swam.ModuleLoader
import swam.binary.ModuleParser
import swam.syntax.Module
import swam.validation.Validator
import swam.binary.ModuleStream
import swam.binary.WasmCodec

import fs2.Stream

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.StreamConverters.*

class FunctionalityTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Test Swam") {
    val uri = this.getClass.getResource("/sturdopt/example").toURI
    Files.list(Paths.get(uri)).toScala(List).filter(p => p.toString.endsWith(".wasm")).sorted.headOption.foreach { p =>
      val mod = Parsing.fromBinary(p)
      //WasmCodec.section.encode(mod)
    }
  }

  test("Random tests") {
    val testval = Vector(3,3,3,3,3,5,5,2,2,2).foldLeft(Vector.empty[(Int, Int)]) {
      case (acc :+ (count, elem), curr) if curr == elem =>
        acc :+ (count+1, elem)
      case (acc, curr) =>
        acc :+ (1, curr)
    }
    println(testval)
  }

  test("Serializer test") {
    val uri = this.getClass.getResource("/sturdopt/example").toURI
    Files.list(Paths.get(uri)).toScala(List).filter(p => p.toString.endsWith(".wasm")).sorted.headOption.foreach { p =>
      val mod = Parsing.fromBinary(p)
      println(mod)
      val mod_bytes = Serializing.serialize(mod).toArray
      assert(mod == Parsing.fromBytes(mod_bytes))
      assert(mod.copy(funcs = Vector.empty) != Parsing.fromBytes(mod_bytes))
    }
  }
}