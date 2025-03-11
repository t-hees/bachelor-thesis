package sturdopt

import sturdopt.Parsing.WasmParseTimeout
import sturdopt.Serializing.prettyPrintModule
import sturdopt.TestUtil.{timedTest, wasmbenchFiles}
import sturdopt.optimizations.{ConstantsOptimization, DeadcodeOptimization, DropsOptimization}
import sturdy.effect.SturdyFailure
import swam.syntax.{Block, Func, If, Inst, Loop, Module}

import java.io.FileWriter
import scala.concurrent.duration.*

class Benchmark extends org.scalatest.funsuite.AnyFunSuite {

  /**
   * Counts instructions of all functions of the module
   */
  def countInst(mod: Module): Int =
    def instVisitor(inst: Inst): Int = inst match
      case If(_, thenInstr, elseInstr) => 1 + (thenInstr ++ elseInstr).map(instVisitor).sum
      case Block(_, blockInstr) => 1 + blockInstr.map(instVisitor).sum
      case Loop(_, loopInstr) => 1 + loopInstr.map(instVisitor).sum
      case _ => 1

    mod.funcs.map((f: Func) => f.body.foldLeft(0)((acc, inst) => acc + instVisitor(inst))).sum

  /**
   * Apply every optimization in order, validate the result and write benchmark results to csv file
   */
  test("Write benchmark") {
    //val wasmbenchFiles = TestUtil.wasmbenchFiles.drop(368)
    var timeoutCounter = 0
    val fileWriter =  new FileWriter(s"${System.currentTimeMillis()}.csv", true)
    wasmbenchFiles.zipWithIndex.foreach { (p, idx) =>
      try {
        println(s"${idx}/${wasmbenchFiles.size - 1}: $p")
        val testFun: () => Unit = () => {
          try {
            val mod = Parsing.fromBinary(p)

            val (constRes, constResDrops) = ConstantsOptimization.replaceConstantsVerbose(mod)
            //val reparsedConstResult = Parsing.fromBytes(Serializing.serialize(constRes)) // validate
            val (deadRes, deadResDrops, deadResUnreachables) = DeadcodeOptimization.eliminateDeadcodeVerbose(constRes)
            //val reparsedDeadResult = Parsing.fromBytes(Serializing.serialize(deadRes)) // validate
            val (dropsRes, dropsResDrops) = DropsOptimization.eliminateDropsVerbose(deadRes)
            val reparsedResult = Parsing.fromBytes(Serializing.serialize(dropsRes)) // validate

            val outputValue = s"$idx, ${p.getFileName.toString}, ${countInst(mod)}, ${countInst(constRes)}, $constResDrops, ${countInst(deadRes)}, $deadResDrops, $deadResUnreachables, ${countInst(dropsRes)}, $dropsResDrops"
            println(outputValue)
            fileWriter.write(outputValue + "\n")
            fileWriter.flush()
          } catch {
            case e: WasmParseTimeout => throw InterruptedException("Parsing timed out")
          }
        }

        if (!timedTest(testFun, 10.minutes)) then timeoutCounter += 1
        println(s"Total amount of timed out files: ${timeoutCounter}/${wasmbenchFiles.size}")
      } catch {
        case e: InterruptedException =>
          fileWriter.close()
          sys.exit(0)
        case e: SturdyFailure => // do nothing for now. Somehow 815ea8d1c2211988c71ab8a80e0e4a5450f1b77238c28e70e8f301f060b3d4ca with idx 304 gives Sturdy Failure for
          println(s"Sturdy failure for file $p") // deadcode analysis after constant, even though the result validates correctly ???}
      }
    }
  }
}