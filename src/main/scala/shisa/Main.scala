package shisa

import scala.language.implicitConversions

import java.io.PrintWriter
import java.nio.file._

import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters._
import scala.util.chaining._

import ShisaIo._

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

  val dq            = '"'
  val getVersion    = () => Exec.execStr(s"scala -2.13.head -e 'println(scala.util.Properties.scalaPropOrNone(${dq}maven.version.number$dq).get)'")
  val getVersionErr = (res: Exec.Result) => sys.error(s"Fail: ${res.exitCode}, lines:\n  ${res.lines.mkString("\n  ")}")

  def getVersionOr(alt: PartialFunction[Exec.Result, String]) = getVersion() match {
    case Exec.Result(0, List(s)) => s
    case res                     => alt.applyOrElse(res, getVersionErr)
  }

  // if we get extra lines from Coursier, run again
  lazy val _2_13_head = getVersionOr { case Exec.Result(0, _) => getVersionOr(PartialFunction.empty) }

       val scalac2 = "scalac -deprecation"
  lazy val scalac3 = {
    Exec.execStr("dotc") match { // make sure dotc is fresh, so we don't leak building output
      case Exec.Result(0, _) =>
      case res               => sys.error(s"Fail: $res, lines:\n  ${res.lines.mkString("\n  ")}")
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
        InvokeCompiler.doCompileLines(sourceFile, combinations)
      } else {
        combinations.foreach(InvokeCompiler.doCompile(sourceFile, _))
      }
    }
  }
}

sealed abstract class CompileFile(val src: Path) {
  val name          = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
  val dir           = createDirs(src.resolveSibling(name))
  val targetDir     = Paths.get("target").resolve(dir)
  def chkPath: Path
  lazy val chk      = new PrintWriter(Files.newBufferedWriter(chkPath), true)
}

final case class CompileFile1(_src: Path, id: String) extends CompileFile(_src) {
  val out     = targetDir.resolve(s"$name.$id")
  val chkPath = dir.resolve(s"$name.$id.check")
}

final case class CompileFileLine(_src: Path, _idx: Int) extends CompileFile(_src) {
  val outD    = createDirs(targetDir.resolve(name))
  val idx     = if (_idx < 10) s"0${_idx}" else s"${_idx}"
  val chkPath = dir.resolve(s"$name.$idx.check")
  val src2    = outD.resolve(s"$name.$idx.scala")
  val out     = (id: String) => outD.resolve(s"$name.$id.$idx")
}

object InvokeCompiler {
  def doCompile(sourceFile: Path, invoke: Invoke): Unit = {
    val file = CompileFile1(sourceFile, invoke.id)
    val CompileResult(exitCode, lines) = invoke.compile1(file.src, file.out)
    val writeBody = s"// exitCode: $exitCode" +: lines
    writeBody.foreach(file.chk.println)
    file.chk.close()
  }

  def doCompileLines(sourceFile: Path, combinations: Seq[Invoke]) = {
    val re   = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r
    val (setup, base, input) = Files.readString(sourceFile) match {
      case re(setup0, base, cases) =>
        (setup0.linesIterator.map(_.trim).mkString("\n"), base, cases.linesIterator.toList)
    }
    def emptyOrCommented(s: String) = s.isEmpty || s.startsWith("//")
    input.iterator.zipWithIndex.filter(!_._1.trim.pipe(emptyOrCommented)).foreach { case (line, _idx) =>
      val file = CompileFileLine(sourceFile, _idx)
      println(s"    Testing ${file.src2}")

      val body = Array.fill(input.size)("")
      body(_idx) = line
      Files.writeString(file.src2, s"$setup\nclass Test $base{\n${body.mkString("\n")}\n}\n")

      file.chk.println(s"// src: $line")

      var prevRes = CompileResult(-127, Nil)
      val summary = ListBuffer.empty[String]
      combinations.foreach { case invoke @ Invoke(id, _) =>
        val res = invoke.compile1(file.src2, file.out(id))
        val result = res.statusPadded
        val writeBody = if (res == prevRes)
          Seq(f"// $id%-9s $result <no change>")
        else
          Seq(f"// $id%-9s $result") ++ (if (res.lines.isEmpty) Nil else res.lines :+ "")
        writeBody.foreach(file.chk.println)
        prevRes = res
        summary += result
      }
      file.chk.println()
      file.chk.println(summary.mkString(" "))
      file.chk.close()
    }
  }
}

final case class Invoke(id: String, cmd: String) {
  def compile1(src: Path, out: Path): CompileResult = {
    val execRes = Exec.execStr(s"$cmd -d ${createDirs(out)} $src")
    val compRes = execRes match {
      case Exec.Result(0, Nil)     => CompileResult(0, Nil)
      case Exec.Result(0, lines)   => CompileResult(0, lines)
      case Exec.Result(err, lines) => CompileResult(err,  lines)
    }
    println(s"${compRes.statusPadded} | $cmd")
    compRes
  }
}

final case class CompileResult(exitCode: Int, lines: List[String]) {
  def statusPadded = this match {
    case CompileResult(0, Nil) => "ok   "
    case CompileResult(0, _)   => "warn "
    case CompileResult(_, _)   => "error"
  }
  def assertNoErrors(): Unit = assert(exitCode == 0, s"$exitCode: " + lines)
}

object ShisaIo {
  def createDirs(dir: Path) = {
    // Files.createDirectories returns the created directory...
    // but (sometimes? on Travis CI at least, compared to locally) as an absolute Path
    // so do this instead
    Files.createDirectories(dir)
    dir
  }
}
