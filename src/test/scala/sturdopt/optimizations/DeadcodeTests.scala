package sturdopt.optimizations

import sturdopt.Parsing
import sturdopt.Parsing.WasmParseTimeout
import sturdopt.Serializing
import sturdopt.Serializing.prettyPrintModule
import sturdopt.TestfilePaths.{deadcodeFiles, wasmbenchFiles}

import java.nio.file.{Files, Paths, StandardOpenOption}

class DeadcodeTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Manual test assertions") {
    deadcodeFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
      val result = DeadcodeOptimization.eliminateDeadcode(Parsing.fromText(p))
      val expected = Parsing.fromText(Paths.get(p.toString + ".expected"))
      assert(result == expected)
    }
  }

  test("Validate after optimization") {
    wasmbenchFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
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

  test("Write to file test") {
    wasmbenchFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
      val output_path = p.toString + ".optimized"
      val mod = Parsing.fromBinary(p)
      val result = DeadcodeOptimization.eliminateDeadcode(mod)
      Files.write(Paths.get(output_path), Serializing.serialize(result).toArray, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    }
  }
}