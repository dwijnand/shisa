package shisa

import java.io.PrintWriter
import java.nio.file._

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.sys.process._
import scala.util.chaining._

object Main {
  def main(args: Array[String]): Unit = {
    val sourceFiles = args.toList match {
      case Nil  => Files.find(Paths.get("tests"), 10, (p, _) => p.toString.endsWith(".scala"))
        .toScala(List)
        .tap(xs => println(s"Files: ${xs.mkString("[", ", ", "]")}"))
      case argv  => argv.map(Paths.get(_)).map { p =>
        if (p.isAbsolute) Paths.get("").toAbsolutePath.relativize(p)
        else p
      }
    }

    val missing = sourceFiles.filter(!Files.exists(_))
    if (missing.nonEmpty)
      sys.error(s"Missing source files: ${missing.mkString("[", ", ", "]")}")

    run(sourceFiles)
  }

  val getVersion    = () => execStr("scala -2.13.head -e println(scala.util.Properties.versionNumberString)")
  val getVersionErr = (res: ExecResult) => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")

  def getVersionOr(alt: PartialFunction[ExecResult, String]) = getVersion() match {
    case ExecResult(0, List(s)) => s
    case res                    => alt.applyOrElse(res, getVersionErr)
  }

  // if we get extra lines from Coursier, run again
  lazy val _2_13_head = getVersionOr { case ExecResult(0, _) => getVersionOr(PartialFunction.empty) }

       val scalac2 = "scalac -deprecation"
  lazy val scalac3 = {
    execStr("dotc") match { // make sure dotc is fresh, so we don't leak building output
      case ExecResult(0, _) =>
      case res              => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")
    }
    "dotc -migration -color:never -explain"
  }

  val combinations = Seq(
    Invoke("2.13-base", s"$scalac2 -2.13.3"),
    Invoke("2.13-head", s"$scalac2 -${_2_13_head}"),
    Invoke("2.13-new",  s"$scalac2 -${_2_13_head} -Xsource:3"),
    Invoke("3.0-old",   s"$scalac3 -source 3.0-migration"),
    Invoke("3.0",       s"$scalac3"), // assumes -source 3.0 is the default
    Invoke("3.1-migr",  s"$scalac3 -source 3.1-migration"),
    Invoke("3.1",       s"$scalac3 -source 3.1"),
  )

  def run(sourceFiles: List[Path]) = {
    sourceFiles.foreach { sourceFile =>
      println(s"  Testing $sourceFile")
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
    val ExecResult(exitCode, lines) = execStr(s"$cmd -d $out $sourceFile")
    Files.write(chk, (s"// exitCode: $exitCode" +: lines).asJava)
  }

  def doCompileLines(sourceFile: Path, combinations: Seq[Invoke]) = {
    val name = sourceFile.getFileName.toString.stripSuffix(".lines.scala")
    val dir  = sourceFile.resolveSibling(name)
    val outD = Paths.get("target").resolve(dir).resolve(name)
    Files.createDirectories(dir)
    Files.createDirectories(outD)
    val re   = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r
    val (setup, base, input) = Files.readString(sourceFile) match {
      case re(setup0, base, cases) =>
        (setup0.linesIterator.map(_.trim).mkString("\n"), base, cases.linesIterator.toList)
    }
    def emptyOrCommented(s: String) = s.isEmpty || s.startsWith("//")
    input.iterator.zipWithIndex.filter(!_._1.trim.pipe(emptyOrCommented)).foreach { case (line, _idx) =>
      val idx = if (_idx < 10) s"0${_idx}" else s"${_idx}"
      val src = outD.resolve(s"$name.$idx.scala")
      val chkP = dir.resolve(s"$name.$idx.check")

      val body = Array.fill(input.size)("")
      body(_idx) = line
      Files.writeString(src, s"$setup\nclass Test $base{\n${body.mkString("\n")}\n}\n")

      val chk = new PrintWriter(Files.newBufferedWriter(chkP), true)
      chk.println(s"// src: $line")

      var prevRes = ExecResult(-127, Nil)
      val summary = ListBuffer.empty[String]
      combinations.foreach { case Invoke(id, cmd) =>
        val out = outD.resolve(s"$name.$id.$idx")
        Files.createDirectories(out)
        val res = execStr(s"$cmd -d $out $src")
        val result = res match {
          case ExecResult(0, Nil) => "ok   "
          case ExecResult(0, _)   => "warn "
          case ExecResult(_, _)   => "error"
        }
        val linesAndPad = if (res.lines.isEmpty) Nil else res.lines :+ ""
        val writeBody =
          if (res == prevRes) Seq(f"// $id%-9s $result <no change>")
          else f"// $id%-9s $result" +: linesAndPad

        writeBody.foreach(chk.println)

        prevRes = res
        summary += result
      }
      chk.println()
      chk.println(summary.mkString(" "))
    }
  }

  def execStr(s: String): ExecResult = {
    val buff = new ListBuffer[String]
    val exit = Process(s) ! ProcessLogger(buff += _, buff += _)
    println(s"$s => $exit")
    ExecResult(exit, buff.toList)
  }
}

final case class Invoke(id: String, cmd: String)
final case class ExecResult(exitCode: Int, lines: List[String])
