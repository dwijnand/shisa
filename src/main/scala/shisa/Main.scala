package shisa

import java.nio.file.{ Files, Path, Paths }

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.sys.process._

object Main {
  def main(args: Array[String]): Unit = {
    val sourceFile = args match {
      case Array(sourceFile) => Paths.get(sourceFile)
      case _                 => sys.error(s"expected file, got $args")
    }

    val scalac2 = "scalac -deprecation"
    val scalac3 = "dotc -migration -color:never -explain"

    val _2_13_head = execStr("scala -2.13.head -e println(scala.util.Properties.versionNumberString)") match {
      case ExecResult(_, 0, Seq(s)) => s
      case res                      => sys.error(s"Fail: $res")
    }

    // More combinations?
    // -Xlint:eta-sam         The Java-defined target interface for eta-expansion was not annotated @FunctionalInterface.
    // -Xlint:eta-zero        Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.
    val combinations = Seq(
      Invoke("2.13-base", s"$scalac2 -2.13.1"),
      Invoke("2.13-head", s"$scalac2 -${_2_13_head}"),
      Invoke("2.13-new",  s"$scalac2 -${_2_13_head} -Xsource:2.14"),
      Invoke("3.0-old",   s"$scalac3 -language:Scala2Compat"),
      Invoke("3.0",       s"$scalac3"),
      Invoke("3.1",       s"$scalac3 -strict"),
    )

    combinations.foreach(run(_, sourceFile))
  }

  def run(inv: Invoke, sourceFile: Path) = {
    import inv.{ id, cmd }
    val checkFile = sourceFile.resolveSibling(sourceFile.getFileName.toString.stripSuffix(".scala") + s".$id.check")
    val res = execStr(s"$cmd $sourceFile")
    println(res)
    import res.{ exitCode, lines }
    Files.write(checkFile, (s"// exitCode: $exitCode" +: lines).asJava)
  }

  def tokenise(s: String) = s.split(' ').toSeq

  def execStr(s: String) = exec(tokenise(s))

  def exec(argv: Seq[String]): ExecResult = {
    val buff = new ListBuffer[String]
    val exit = Process(argv) ! ProcessLogger(buff += _, buff += _)
    ExecResult(argv, exit, buff.toList)
  }
}

final case class Invoke(id: String, cmd: String)

final case class ExecResult(argv: Seq[String], exitCode: Int, lines: Seq[String]) {
  override def toString = s"${argv.mkString(" ")} => $exitCode"
}

