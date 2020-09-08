package shisa

import scala.language.implicitConversions

import java.io.PrintWriter
import java.nio.file._

import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters._
import scala.util.chaining._

import ShisaIo._

abstract class MainClass {
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

  def combinations: Seq[Invoke]

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
  def chkOpt: OpenOption
  def chkOpts       = List(StandardOpenOption.CREATE, chkOpt, StandardOpenOption.WRITE)
  lazy val chk      = new PrintWriter(Files.newBufferedWriter(chkPath, chkOpts: _*), true)
}

final case class CompileFile1(_src: Path, id: String) extends CompileFile(_src) {
  val out     = createDirs(targetDir.resolve(s"$name.$id"))
  val chkPath = dir.resolve(s"$name.$id.check")
  def chkOpt  = StandardOpenOption.TRUNCATE_EXISTING
}

final case class CompileFileLine(_src: Path, _idx: Int) extends CompileFile(_src) {
  val outD    = createDirs(targetDir.resolve(name))
  val idx     = if (_idx < 10) s"0${_idx}" else s"${_idx}"
  val chkPath = dir.resolve(s"$name.$idx.check")
  val src2    = outD.resolve(s"$name.$idx.scala")
  val summF   = outD.resolve(s"$name.$idx.summary")
  val prevOkF = outD.resolve(s"$name.$idx.prevOk")
  val summ    = if (Files.exists(summF)) try Files.readString(summF) finally Files.delete(summF) else ""
  val prevOk  = if (Files.exists(prevOkF)) try true finally Files.delete(prevOkF) else false
  def chkOpt  = if (summ == "") StandardOpenOption.TRUNCATE_EXISTING else StandardOpenOption.APPEND
  val out     = (id: String) => createDirs(outD.resolve(s"$name.$id.$idx"))
}

object InvokeCompiler {
  def doCompile(sourceFile: Path, invoke: Invoke): Unit = {
    val file = CompileFile1(sourceFile, invoke.id)
    val CompileResult(exitCode, lines) = invoke.compile1(file.src, file.out)
    val writeBody = s"// exitCode: $exitCode" +: lines
    (writeBody.init :+ writeBody.last.stripLineEnd).foreach(file.chk.println)
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
      print(s"    Testing ${file.src2}")

      val body = Array.fill(input.size)("")
      body(_idx) = line
      Files.writeString(file.src2, s"$setup\nclass Test $base{\n${body.mkString("\n")}\n}\n")

      val summaries = ListBuffer.empty[String]
      if (file.summ == "") {
        file.chk.println(s"// src: $line")
      } else {
        summaries += file.summ
      }

      var prevRes = CompileResult(if (file.prevOk) 0 else -127, Nil)

      combinations.foreach { invoke =>
        val id = invoke.id
        val res = invoke.compile1(file.src2, file.out(id))
        val result = res.statusPadded
        val writeBody = if (res == prevRes)
          Seq(f"// $id%-9s $result <no change>")
        else
          Seq(f"// $id%-9s $result") ++ (if (res.lines.isEmpty) Nil else res.lines :+ "")
        writeBody.foreach(file.chk.println)
        prevRes = res
        summaries += result
        print('.')
      }

      val summary = summaries.mkString(" ")
      if (file.summ == "") {
        Files.writeString(file.summF, summary)
        if (prevRes.exitCode == 0) Files.writeString(file.prevOkF, "")
      } else {
        file.chk.println()
        file.chk.println(summary)
      }

      file.chk.close()
      println("")
    }
  }
}

trait Invoke {
  def id: String
  def compile1(src: Path, out: Path): CompileResult
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
