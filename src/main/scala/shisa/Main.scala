package shisa

import java.nio.file._
import java.nio.file.StandardOpenOption._

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

    val getVersion = "scala -2.13.head -e println(scala.util.Properties.versionNumberString)"
    lazy val _2_13_head = execStr(getVersion).tap(println) match {
      case ExecResult(_, 0, Seq(s)) => s
      case ExecResult(_, 0, _)      => execStr(getVersion).tap(println) match { // got extra lines from Coursier, run again
        case ExecResult(_, 0, Seq(s)) => s
        case res                      => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")
      }
      case res                      => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")
    }

    val scalac2 = "scalac -deprecation"
    lazy val scalac3 = {
      execStr("dotc") match { // make sure dotc is fresh, so we don't leak building output
        case ExecResult(_, 0, _) => ()
        case res                 => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")
      }
      "dotc -migration -color:never -explain"
    }

    // More combinations?
    // -Xlint:eta-sam         The Java-defined target interface for eta-expansion was not annotated @FunctionalInterface.
    // -Xlint:eta-zero        Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.
    // https://github.com/lampepfl/dotty/issues/8571 dotty options
    val combinations = Seq(
      Invoke("2.13-base", s"$scalac2 -2.13.2"),
      Invoke("2.13-head", s"$scalac2 -${_2_13_head}"),
      Invoke("2.13-new",  s"$scalac2 -${_2_13_head} -Xsource:3"),
      Invoke("3.0-old",   s"$scalac3 -source 3.0-migration"),
      Invoke("3.0",       s"$scalac3"), // assumes -source 3.0 is the default
      Invoke("3.1-migr",  s"$scalac3 -source 3.1-migration"),
      Invoke("3.1",       s"$scalac3 -source 3.1"),
    )

    sourceFiles.foreach { sourceFile =>
      if (sourceFiles.sizeIs > 1) println(s"  Testing $sourceFile")
      if (sourceFile.toString.endsWith(".lines.scala")) {
        doCompileLines(sourceFile, combinations)
      } else {
        combinations.foreach { case Invoke(id, cmd) => doCompile(id, cmd, sourceFile) }
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

  def doCompileLines(sourceFile: Path, combinations: Seq[Invoke]) = {
    val name = sourceFile.getFileName.toString.stripSuffix(".lines.scala")
    val dir  = Files.createDirectories(sourceFile.resolveSibling(name))
    val outD = Files.createDirectories(Paths.get("target").resolve(dir).resolve(name))
    val re   = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r
    val (setup0, base, cases) = Files.readString(sourceFile) match {
      case re(setup, base, cases) => (setup, base, cases)
    }
    val setup = setup0.linesIterator.map(_.trim).mkString("\n")
    val input = cases.linesIterator.toList
    val count = input.size
    input.iterator.zipWithIndex.filter { case (l, _) => !l.trim.pipe(s => s.isEmpty || s.startsWith("//")) }.foreach { case (line, _idx) =>
      val body = Array.fill(count)("")
      body(_idx) = line
      val idx = if (_idx < 10) s"0${_idx}" else s"${_idx}"
      val src = outD.resolve(s"$name.$idx.scala")
      val chk = dir.resolve(s"$name.$idx.check")
      Files.writeString(src, s"${setup}\nclass Test $base{\n${body.mkString("\n")}\n}\n")
      Files.writeString(chk, s"// src: $line\n", CREATE, TRUNCATE_EXISTING)
      var prevExitCode = -127
      var prevLines    = Seq.empty[String]
      val summary      = ListBuffer.empty[String]
      combinations.foreach { case Invoke(id, cmd) =>
        val out = Files.createDirectories(outD.resolve(s"$name.$id.$idx"))
        val ExecResult(_, exitCode, lines) = execStr(s"$cmd -d $out $src").tap(println)
        val result      = if (exitCode == 0) if (lines.isEmpty) "ok   " else "warn " else "error"
        val linesAndPad = if (lines.isEmpty) Nil else lines :+ ""
        val writeBody =
          if (prevExitCode != exitCode || prevLines != lines) f"// $id%-9s $result" +: linesAndPad
          else Seq(f"// $id%-9s $result <no change>")
        Files.write(chk, writeBody.asJava, CREATE, APPEND)
        prevExitCode = exitCode
        prevLines    = lines
        summary     += result
      }
      Files.write(chk, Seq("", summary.mkString(" ")).asJava, CREATE, APPEND)
    }
  }

  def tokenise(s: String) = s.split(' ').toSeq
  def execStr(s: String)  = exec(tokenise(s))

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

