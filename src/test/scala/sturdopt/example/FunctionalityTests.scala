package sturdopt.example

import sturdopt.Parsing
import sturdopt.Serializing

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.StreamConverters.*

import sturdopt.optimizations.DeadcodeOptimization

class FunctionalityTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Serializer test") {
    val uri = this.getClass.getResource("/sturdopt/example").toURI
    Files.list(Paths.get(uri)).toScala(List).filter(p => p.toString.endsWith(".wasm")).sorted.headOption.foreach { p =>
      val mod = Parsing.fromBinary(p)
      val mod_bytes = Serializing.serialize(mod).toArray
      assert(mod == Parsing.fromBytes(mod_bytes))
    }
  }

  test("Deadcode test") {
    val uri = this.getClass.getResource("/sturdopt/example").toURI
    Files.list(Paths.get(uri)).toScala(List).filter(p => p.toString.endsWith(".wasm")).sorted.headOption.foreach { p =>
      val mod = Parsing.fromBinary(p)
      println(DeadcodeOptimization.eliminateDeadcode(mod))

    }
  }
}