package sturdopt

import sturdopt.Parsing.WasmParseTimeout
import sturdopt.{Parsing, Serializing}

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.jdk.StreamConverters.*

object TestfilePaths {
  val consideredFilesNames = Files.lines(Paths.get(this.getClass.getResource("/wasmbench-considered-files-60s.txt").toURI)).toArray.map(_.toString)
  val wasmbenchFiles = Files.list(Paths.get(this.getClass.getResource("/wasmbench/filtered").toURI))
    .toScala(List).filter(p => consideredFilesNames.exists(p.endsWith(_))).sorted((a, b) => java.lang.Long.compare(Files.size(a), Files.size(b)))
  val deadcodeFiles = Files.list(Paths.get(this.getClass.getResource("/sturdopt/optimizations/deadcode").toURI))
    .toScala(List).filter(p => p.toString.endsWith(".wat")).sorted
}

class GeneralTests extends org.scalatest.funsuite.AnyFunSuite {
  test("Serializer test") {
    TestfilePaths.wasmbenchFiles.zipWithIndex.foreach { (p, idx) =>
      println(s"${idx}/${TestfilePaths.wasmbenchFiles.size-1}: $p")
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
