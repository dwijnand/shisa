package shisa

import java.nio.file.{ Files, Path, Paths }

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.sys.process._
import scala.util.chaining._

object Main {
  def main(args: Array[String]): Unit = {
    val sourceFiles = args.toSeq match {
      case Seq() => Files.find(Paths.get("tests"), 10, (p, _) => p.toString.endsWith(".scala"))
        .toScala(Seq)
        .tap(xs => println(s"Files: ${xs.mkString("[", ", ", "]")}"))
      case argv  => argv.map(Paths.get(_))
    }

    val missing = sourceFiles.filter(!Files.exists(_))
    if (missing.nonEmpty)
      sys.error(s"Missing source files: ${missing.mkString("[", ", ", "]")}")

    val _2_13_head = execStr("scala -2.13.head -e println(scala.util.Properties.versionNumberString)") match {
      case ExecResult(_, 0, Seq(s)) => s
      case res                      => sys.error(s"Fail: $res")
    }

    val scalac2 = "scalac -deprecation"
    val scalac3 = "dotc -migration -color:never -explain"

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

    for {
      sourceFile <- sourceFiles
      Invoke(id, cmd) <- combinations
    } run(id, cmd, sourceFile)
  }

  def run(id: String, cmd: String, sourceFile: Path) = {
    val name = sourceFile.getFileName.toString.stripSuffix(".scala")
    val dir = Files.createDirectories(sourceFile.resolveSibling(name))
    val out = Files.createDirectories(dir.resolve(s"$name.$id.out"))
    val chk = dir.resolve(s"$name.$id.check")
    val ExecResult(_, exitCode, lines) = execStr(s"$cmd -d $out $sourceFile").tap(println)
    Files.write(chk, (s"// exitCode: $exitCode" +: lines).asJava)
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

