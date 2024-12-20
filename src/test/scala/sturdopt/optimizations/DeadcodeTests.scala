package sturdopt.optimizations

import sturdopt.{Parsing, Serializing, TestUtil}
import sturdopt.Parsing.WasmParseTimeout
import sturdopt.Serializing.prettyPrintModule
import sturdopt.TestUtil.{deadcodeFiles, timedTest, wasmbenchFiles}

import java.nio.file.{Files, Paths, StandardOpenOption}

class DeadcodeTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Manual test assertions") {
    deadcodeFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
      val result = DeadcodeOptimization.eliminateDeadcode(Parsing.fromText(p))
      //prettyPrintModule(result)
      val expected = Parsing.fromText(Paths.get(p.toString + ".expected"))
      assert(result == expected)
    }
  }

  test("Validate after optimization") {
    //val wasmbenchFiles = TestUtil.wasmbenchFiles.drop(461)
    wasmbenchFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${wasmbenchFiles.size-1}: $p")
      val testFun: () => Unit = () => {
        try {
          val mod = Parsing.fromBinary(p)
          //prettyPrintModule(mod)
          val result = DeadcodeOptimization.eliminateDeadcode(mod)
          //prettyPrintModule(result)
          val reparsedResult = Parsing.fromBytes(Serializing.serialize(result))
        } catch {
          case e: WasmParseTimeout => println("Parsing timed out")
        }
      }
      timedTest(testFun)
    }
  }

  test("Write to file test") {
    val files = wasmbenchFiles.zipWithIndex.filter{ (p, idx) => idx == 336 }
    files.foreach { (p, idx) =>
      println(s"${idx}/${deadcodeFiles.size-1}: $p")
      val output_path = p.toString + ".optimized"
      val mod = Parsing.fromBinary(p)
      val result = DeadcodeOptimization.eliminateDeadcode(mod)
      Files.write(Paths.get(output_path), Serializing.serialize(result))
    }
  }
}

