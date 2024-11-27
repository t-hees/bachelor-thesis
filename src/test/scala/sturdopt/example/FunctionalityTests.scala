package sturdopt.example

import sturdopt.Parsing
import sturdopt.Parsing.WasmParseTimeout
import sturdopt.Serializing
import sturdopt.Serializing.prettyPrintModule

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.StreamConverters.*
import sturdopt.optimizations.DeadcodeOptimization

class FunctionalityTests extends org.scalatest.funsuite.AnyFunSuite {
  val consideredFilesNames = Files.lines(Paths.get(this.getClass.getResource("/wasmbench-considered-files-60s.txt").toURI)).toArray.map(_.toString)
  val wasmbenchFiles = Files.list(Paths.get(this.getClass.getResource("/wasmbench/filtered").toURI))
    .toScala(List).filter(p => consideredFilesNames.exists(p.endsWith(_))).sorted
  val manualTestFiles = Files.list(Paths.get(this.getClass.getResource("/sturdopt/example").toURI))
    .toScala(List).filter(p => p.toString.endsWith(".wasm")).sorted
  val testFiles = manualTestFiles ++ wasmbenchFiles

  test("Serializer test") {
    testFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${testFiles.size-1}: $p")
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
      println(s"${idx}/${testFiles.size-1}: $p")
      try {
        val mod = Parsing.fromBinary(p)
        //prettyPrintModule(mod)
        val result = DeadcodeOptimization.eliminateDeadcode(mod)
        //prettyPrintModule(result)
        val reparsedResult = Parsing.fromBytes(Serializing.serialize(result).toArray)
      } catch {
        case e: WasmParseTimeout => println("Parsing timed out")
      }
    }
  }

  test("Deadcode write to file test") {
    testFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${testFiles.size-1}: $p")
      val output_path = p.toString + ".optimized"
      val mod = Parsing.fromBinary(p)
      val result = DeadcodeOptimization.eliminateDeadcode(mod)
      Files.write(Paths.get(output_path), Serializing.serialize(result).toArray, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    }
  }
}