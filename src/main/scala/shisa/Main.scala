package shisa

import java.io._
import java.nio.file._

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.sys.process._
import scala.util.Using
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

    val getVersion = "scala -2.13.head -e println(scala.util.Properties.versionNumberString)"
    lazy val _2_13_head = execStr(getVersion).tap(println) match {
      case ExecResult(_, 0, Seq(s)) => s
      case ExecResult(_, 0, _)      => execStr(getVersion).tap(println) match { // got extra lines from Coursier, run again
        case ExecResult(_, 0, Seq(s)) => s
        case res                      => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")
      }
      case res                      => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")
    }

    val scalac2 = "scalac"
    val scalac3 = "dotc"
    val scala2  = "scala"
    val scala3  = "dotr -Djava.util.logging.config.file=log.properties"
    val opts2   = "-deprecation"
    val opts3   = "-migration -color:never -explain"

    // More combinations?
    // -Xlint:eta-sam         The Java-defined target interface for eta-expansion was not annotated @FunctionalInterface.
    // -Xlint:eta-zero        Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.
    // https://github.com/lampepfl/dotty/issues/8571 dotty options
    val combinations = Seq(
      Invoke.mk("2.13-base", scalac2, scala2, opts2, s"-2.13.1"),
      Invoke.mk("2.13-head", scalac2, scala2, opts2, s"-${_2_13_head}"),
      Invoke.mk("2.13-new",  scalac2, scala2, opts2, s"-${_2_13_head} -Xsource:2.14"),
      Invoke.mk("3.0-old",   scalac3, scala3, opts3, s"-language:Scala2Compat"),
      Invoke.mk("3.0",       scalac3, scala3, opts3, ""),
      Invoke.mk("3.1",       scalac3, scala3, opts3, "-strict"),
    )

    sourceFiles.foreach { sourceFile =>
      if (sourceFiles.sizeIs > 1) println(s"  Testing $sourceFile")
      combinations.zipWithIndex.foreach { case (Invoke(id, compile, interpret), idx) =>
        if (sourceFile.toString.endsWith(".lines.scala"))
          doInterpret(id, interpret, sourceFile)
        else
          doCompile(id, compile, sourceFile)
      }
    }
  }

  def doCompile(id: String, cmd: String, sourceFile: Path) = {
    val name = sourceFile.getFileName.toString.stripSuffix(".scala")
    val dir = Files.createDirectories(sourceFile.resolveSibling(name))
    val out = Files.createDirectories(Paths.get("target").resolve(dir).resolve(s"$name.$id"))
    val chk = dir.resolve(s"$name.$id.check")
    val ExecResult(_, exitCode, lines) = execStr(s"$cmd -d $out $sourceFile").tap(println)
    Files.write(chk, (s"// exitCode: $exitCode" +: lines).asJava)
  }

  def doInterpret(id: String, cmd: String, sourceFile: Path) = {
    val name = sourceFile.getFileName.toString.stripSuffix(".lines.scala")
    val dir  = Files.createDirectories(sourceFile.resolveSibling(name))
    val chk  = dir.resolve(s"$name.$id.check")
    val Regex = """(?s)(.*)class Test \{(.*)}\n""".r
    val text = Files.readString(sourceFile) match {
      case Regex(setup, cases) => s"$setup\n$cases"
    }
    val input = text.linesIterator.map(_.trim).filter(s => s.nonEmpty && !s.startsWith("//")).toList
    val buff  = new ListBuffer[String]
    val isDotty = !cmd.startsWith("scala")
    val writeIn = (out: OutputStream) => Using.resource(new PrintWriter(out))(pw => input.foreach { s =>
      if (isDotty) buff += s // dotr doesn't print input, unlike intp
      pw.println(s)
    })
    val saveLines = BasicIO.processFully(s => buff += normalize(s))
    val argv = tokenise(s"$cmd")
    val exit = Process(argv).run(new ProcessIO(writeIn, saveLines, saveLines)).exitValue()
    val ExecResult(_, exitCode, lines) = ExecResult(argv, exit, buff.toList).tap(println)
    Files.write(chk, (s"// exitCode: $exitCode" +: lines).asJava)
  }

  def tokenise(s: String) = s.split(' ').toSeq
  def execStr(s: String)  = exec(tokenise(s))

  def exec(argv: Seq[String]): ExecResult = {
    val buff = new ListBuffer[String]
    val exit = Process(argv) ! ProcessLogger(buff += _, buff += _)
    ExecResult(argv, exit, buff.toList)
  }

  val normalize = Function.chain(Seq(
    stripLambdaClassName(_),
    stripIdentityHashCode(_),
  ))

  def stripIdentityHashCode(s: String) =   hashless.replaceAllIn(s, "$1@XXXXXXXX")
  def stripLambdaClassName(s: String)  = lambdaless.replaceAllIn(s, "<function>")

  val   hashless = "([^ ])@[a-fA-F0-9]+".r
  val lambdaless = """[$]*Lambda\$\d+/(?:0x[a-f0-9]{16}|\d+)(@[a-fA-F0-9]+)?""".r
}

final case class Invoke(id: String, compile: String, interpret: String)

object Invoke {
  def mk(id: String, scalac: String, scala: String, opts1: String, opts2: String) =
    Invoke(id, mkStr(scalac, opts1, opts2), mkStr(scala, opts1, opts2))

  private def mkStr(xs: String*) = xs.filter(_.nonEmpty).mkString(" ")
}

final case class ExecResult(argv: Seq[String], exitCode: Int, lines: Seq[String]) {
  override def toString = s"${argv.mkString(" ")} => $exitCode"
}

