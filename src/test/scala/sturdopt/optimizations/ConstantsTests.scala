package sturdopt.optimizations

import sturdopt.Parsing.WasmParseTimeout
import sturdopt.Serializing.prettyPrintModule
import sturdopt.TestUtil.{constantsFiles, timedTest, wasmbenchFiles}
import sturdopt.{Parsing, Serializing, TestUtil}

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.concurrent.duration.*

class ConstantsTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Manual test assertions") {
    constantsFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
      val result = ConstantsOptimization.replaceConstants(Parsing.fromText(p))
      prettyPrintModule(result)
      val expected = Parsing.fromText(Paths.get(p.toString + ".expected"))
      assert(result == expected)
    }
  }

  test("Validate after optimization") {
    //val wasmbenchFiles = TestUtil.wasmbenchFiles.drop(461)
    var timeoutCounter = 0
    wasmbenchFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
      val testFun: () => Unit = () => {
        try {
          val mod = Parsing.fromBinary(p)
          //prettyPrintModule(mod)
          val result = ConstantsOptimization.replaceConstants(mod)
          //prettyPrintModule(result)
          val reparsedResult = Parsing.fromBytes(Serializing.serialize(result))
        } catch {
          case e: WasmParseTimeout => throw InterruptedException("Parsing timed out")
        }
      }
      if (!timedTest(testFun, 5.minutes)) then timeoutCounter += 1
    }
    println(s"Total amount of timed out files: ${timeoutCounter}/${wasmbenchFiles.size}")
  }

  test("Write to file test") {
    val files = wasmbenchFiles.zipWithIndex.filter{ (p, idx) => idx == 336 }
    files.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
      val output_path = p.toString + ".optimized"
      val mod = Parsing.fromBinary(p)
      val result = ConstantsOptimization.replaceConstants(mod)
      Files.write(Paths.get(output_path), Serializing.serialize(result))
    }
  }
}

