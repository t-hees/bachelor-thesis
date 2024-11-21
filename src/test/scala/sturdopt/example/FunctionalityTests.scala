package sturdopt.example

import sturdopt.Parsing
import sturdopt.Parsing.WasmParseTimeout
import sturdopt.Serializing

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.StreamConverters.*
import sturdopt.optimizations.DeadcodeOptimization

class FunctionalityTests extends org.scalatest.funsuite.AnyFunSuite {
  val testfileDirectories = Seq("/sturdopt/example", "/wasmbench/filtered")
  val testFiles: Seq[Path] = testfileDirectories.flatMap { (d: String) =>
    Files.list(Paths.get(this.getClass.getResource(d).toURI)).toScala(List).filter(p => p.toString.endsWith(".wasm"))
  }.sorted

  test("Serializer test") {
    testFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${testFiles.size}: $p")
      try {
        val mod = Parsing.fromBinary(p)
        val mod_bytes = Serializing.serialize(mod).toArray
        assert(mod == Parsing.fromBytes(mod_bytes))
      } catch {
        case e: WasmParseTimeout => println("Parsing timed out")
      }
    }
  }

  test("Deadcode validate after optimization") {
    testFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${testFiles.size}: $p")
      try {
        val mod = Parsing.fromBinary(p)
        val result = DeadcodeOptimization.eliminateDeadcode(mod)
        val reparsedResult = Parsing.fromBytes(Serializing.serialize(result).toArray)
      } catch {
        case e: WasmParseTimeout => println("Parsing timed out")
      }
    }
  }

  test("Deadcode write to file test") {
    testFiles.foreach { p =>
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