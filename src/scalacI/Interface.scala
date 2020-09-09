package shisa

import scala.language.implicitConversions

import java.io.PrintWriter
import java.nio.file._

import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters._
import scala.util.chaining._

final case class CompileResult(exitCode: Int, lines: List[String]) {
  def statusPadded = this match {
    case CompileResult(0, Nil) => "ok   "
    case CompileResult(0, _)   => "warn "
    case CompileResult(_, _)   => "error"
  }
  def assertNoErrors(): Unit = assert(exitCode == 0, s"$exitCode: " + lines)
}

trait Runner {
  def compile1(src: Path): CompileResult
}

trait Invoke extends Runner {
  def id: String
  def cmd: String
  def mkRunner(): Runner
  def compile1(src: Path): CompileResult = mkRunner().compile1(src)
}
