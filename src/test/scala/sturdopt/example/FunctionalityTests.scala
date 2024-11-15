package sturdopt.example

import sturdopt.Parsing
import sturdopt.Serializing

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.StreamConverters.*
import sturdopt.optimizations.DeadcodeOptimization

class FunctionalityTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Serializer test") {
    val uri = this.getClass.getResource("/sturdopt/example").toURI
    Files.list(Paths.get(uri)).toScala(List).filter(p => p.toString.endsWith(".wasm")).sorted.foreach { p =>
      val mod = Parsing.fromBinary(p)
      val mod_bytes = Serializing.serialize(mod).toArray
      assert(mod == Parsing.fromBytes(mod_bytes))
    }
  }

  test("Deadcode test") {
    val uri = this.getClass.getResource("/sturdopt/example").toURI
    Files.list(Paths.get(uri)).toScala(List).filter(p => p.toString.endsWith(".wasm")).sorted.foreach { p =>
      println(p)
      val output_path = p.toString + ".optimized"
      val mod = Parsing.fromBinary(p)
      println(mod)
      val result = DeadcodeOptimization.eliminateDeadcode(mod)
      println(result)
      Files.write(Paths.get(output_path), Serializing.serialize(result).toArray, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    }
  }
}