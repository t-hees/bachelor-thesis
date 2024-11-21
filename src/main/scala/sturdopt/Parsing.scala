package sturdopt

import cats.effect.IO

import java.nio.file.Path
import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.Timer
import fastparse.ParserInputSource
import swam.ModuleLoader
import swam.binary.ModuleParser
import swam.syntax.Module
import swam.text.Command
import swam.text.Compiler
import swam.text.parser.TestScriptParser
import swam.text.unresolved
import swam.validation.Validator
import fs2.Stream
import swam.syntax.Section

import java.io.FileInputStream
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration

object Parsing:
  class WasmParseTimeout(msg: String) extends Exception(msg)

  def fromText(path: Path): Module =
    implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)
    implicit val timer: Timer[IO] = cats.effect.IO.timer(scala.concurrent.ExecutionContext.global)
    try {
      Blocker[IO].use { blocker =>
        for {
          compiler <- Compiler[IO](blocker)
          mod <- compiler.compile(path, blocker)
        } yield mod
      }.timeout(FiniteDuration(10, "s")).unsafeRunSync()
    } catch {
      case e: TimeoutException => throw new WasmParseTimeout(s"Parsing of $path timed out")
    }

  def fromBinary(path: Path): Module =
    implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)
    implicit val timer: Timer[IO] = cats.effect.IO.timer(scala.concurrent.ExecutionContext.global)
    try {
      Blocker[IO].use { blocker =>
        for {
          validator <- Validator[IO](blocker)
          loader = new ModuleLoader[IO]()
          binaryParser = new ModuleParser[IO](validator)
          mod <- binaryParser.parse(loader.sections(path, blocker))
        } yield mod
      }.timeout(FiniteDuration(10, "s")).unsafeRunSync()
    } catch {
      case e: TimeoutException => throw new WasmParseTimeout(s"Parsing of $path timed out")
    }

  def fromBytes(bytes: Array[Byte]): Module =
    implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)
    implicit val timer: Timer[IO] = cats.effect.IO.timer(scala.concurrent.ExecutionContext.global)
    try {
      Blocker[IO].use { blocker =>
        for {
          validator <- Validator[IO](blocker)
          loader = new ModuleLoader[IO]()
          binaryParser = new ModuleParser[IO](validator)
          mod <- binaryParser.parse(loader.sections(bytes))
        } yield mod
      }.timeout(FiniteDuration(10, "s")).unsafeRunSync()
    } catch {
      case e: TimeoutException => throw new WasmParseTimeout(s"Parsing timed out")
    }

  def fromUnresolved(mod: unresolved.Module): Module =
    implicit val cs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)
    implicit val timer: Timer[IO] = cats.effect.IO.timer(scala.concurrent.ExecutionContext.global)
    try {
      Blocker[IO].use { blocker =>
        for {
          compiler <- Compiler[IO](blocker)
          mod <- compiler.compile(mod)
        } yield mod
      }.timeout(FiniteDuration(10, "s")).unsafeRunSync()
    } catch {
      case e: TimeoutException => throw new WasmParseTimeout(s"Parsing of ${mod.id} timed out")
    }
    