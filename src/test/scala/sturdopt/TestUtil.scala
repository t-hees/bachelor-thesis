package sturdopt

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.concurrent.{ExecutionException, TimeoutException}
import scala.concurrent.duration._
import scala.jdk.StreamConverters.*

object TestUtil {
  val consideredFilesNames = Files.lines(Paths.get(this.getClass.getResource("/wasmbench-considered-files-60s.txt").toURI)).toArray.map(_.toString)
  val wasmbenchFiles = Files.list(Paths.get(this.getClass.getResource("/wasmbench/filtered").toURI))
    .toScala(List).filter(p => consideredFilesNames.exists(p.endsWith(_))).sorted((a, b) => java.lang.Long.compare(Files.size(a), Files.size(b)))
  val deadcodeFiles = Files.list(Paths.get(this.getClass.getResource("/sturdopt/optimizations/deadcode").toURI))
    .toScala(List).filter(p => p.toString.endsWith(".wat")).sorted
  val constantsFiles = Files.list(Paths.get(this.getClass.getResource("/sturdopt/optimizations/constants").toURI))
    .toScala(List).filter(p => p.toString.endsWith(".wat")).sorted
  val dropsFiles = Files.list(Paths.get(this.getClass.getResource("/sturdopt/optimizations/drops").toURI))
    .toScala(List).filter(p => p.toString.endsWith(".wat")).sorted

  /**
   * Run test function with timeout
   * @return False if timed out, else True
   */
  def timedTest(testFun: () => Unit, timeLimit: Duration = 60.seconds): Boolean =
    var threadException: Option[Throwable] = None
    try {
      val t = new Thread(new Runnable {
        override def run(): Unit =
          try {
            testFun()
          } catch {
            case e: Throwable => threadException = Some(e)
          }
      })
      t.start()
      t.join(timeLimit.toMillis)
      if (t.isAlive) then
        t.interrupt()
        t.join()
        t.stop()
        println(s"\u001B[33mTest timed out after ${timeLimit.toSeconds} seconds\u001B[0m")
    } catch {
      case e: OutOfMemoryError => System.gc()
    }
    threadException match
      case Some(e: InterruptedException) => false
      case Some(e: Throwable) => throw e
      case None => true
}