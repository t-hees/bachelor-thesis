package sturdopt

import sturdopt.Parsing.WasmParseTimeout
import sturdopt.{Parsing, Serializing}

class GeneralTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Serializer test") {
    TestUtil.wasmbenchFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${TestUtil.wasmbenchFiles.size-1}: $p")
      try {
        val mod = Parsing.fromBinary(p)
        val mod_bytes = Serializing.serialize(mod).toArray
        assert(mod == Parsing.fromBytes(mod_bytes))
      } catch {
        case e: WasmParseTimeout => println("Parsing timed out")
      }
    }
  }
}