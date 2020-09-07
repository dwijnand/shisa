package shisa

import scala.language.implicitConversions

import java.io.{ File, PrintWriter }
import java.nio.file._

import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters._
import scala.util.Try
import scala.util.chaining._

import coursier._

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

import dotty.tools.dotc, dotc.interfaces.{ Diagnostic, SimpleReporter, SourceFile, SourcePosition }

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

  def display(dia: Diagnostic): String = messageAndPos(dia.message, Option(dia.position.orElse(null)), diagnosticLevel(dia))

  // adapted from dotc.reporting.MessageRendering
  def diagnosticLevel(dia: Diagnostic): String = dia.getClass.getSimpleName match {
    case "Error"              => "Error"
    case "Info"               => "Info"
    case "Warning"            => "Warning"
    case "DeprecationWarning" => "Deprecation Warning"
    case     "FeatureWarning" =>     "Feature Warning"
    case   "MigrationWarning" =>   "Migration Warning"
    case   "UncheckedWarning" =>   "Unchecked Warning"
  }

  def messageAndPos(msg: String, pos: Option[SourcePosition], diagnosticLevel: String): String = {
    val sb = new StringBuilder("")
    pos.foreach(pos => sb ++= posStr(pos, diagnosticLevel) ++= EOL)
    pos.filter(pos => absFileExists(pos.source)).fold(sb ++= msg) { pos =>
      val (srcBefore, srcAfter, offset) = sourceLines(pos)
      val marker = columnMarker(pos, offset)
      val err = errorMsg(pos, msg, offset)
      sb ++= (srcBefore ::: marker :: err :: srcAfter).mkString(EOL)
    }
    sb.toString
  }

  def posStr(pos: SourcePosition, diagnosticLevel: String): String = {
    val src    = pos.source
    val path   = src.path
    val line1  = pos.line + 1
    val col    = pos.column
    val file   = s"$path:$line1:$col"
    val errId  = ""
    val kind   = diagnosticLevel
    val prefix = s"-- $errId$kind: $file "
    prefix + "-" * (pageWidth - prefix.length max 0)
  }

  def sourceLines(pos: SourcePosition): (List[String], List[String], Int) = {
    var maxLen = Int.MinValue

    def render(offsetAndLine: (Int, String)): String = {
      val (offset, line) = offsetAndLine
      val lineNbr = src_offsetToLine(pos.source, offset)
      val prefix = s"${lineNbr + 1} |"
      maxLen = math.max(maxLen, prefix.length)
      val lnum = " " * math.max(0, maxLen - prefix.length) + prefix
      lnum + line.stripLineEnd
    }

    def linesFrom(arr: Array[Char]): List[String] = {
      import scala.annotation.switch
      def pred(c: Char) = (c: @switch) match {
        case LF | CR | FF | SU => true
        case _ => false
      }
      val (line, rest0) = arr.span(!pred(_))
      val (_, rest) = rest0.span(pred)
      new String(line) :: { if (rest.isEmpty) Nil else linesFrom(rest) }
    }

    val syntax = pos_linesSlice(pos)
    val lines  = linesFrom(syntax)
    val (before, after) = pos_beforeAndAfterPoint(pos)

    (
      before.zip(lines).map(render),
      after.zip(lines.drop(before.length)).map(render),
      maxLen
    )
  }

  def columnMarker(pos: SourcePosition, offset: Int): String = {
    val prefix  = " " * (offset - 1)
    val padding = pos_startColumnPadding(pos)
    val carets  = if (pos.startLine == pos.endLine) "^" * (1 max pos.endColumn - pos.startColumn) else "^"
    s"$prefix|$padding$carets"
  }

  def errorMsg(pos: SourcePosition, msg: String, offset: Int): String = {
    val padding = msg.linesIterator.foldLeft(pos_startColumnPadding(pos)) { (pad, line) =>
      val lineLength = line.length
      val maxPad     = (0 max pageWidth - offset - lineLength) - offset
      if (maxPad < pad.length) " " * maxPad
      else pad
    }

    msg.linesIterator
      .map { line => " " * (offset - 1) + "|" + (if (line == "") "" else padding + line) }
      .mkString(EOL)
  }

  def src_offsetToLine(src: SourceFile, offset: Int) = 0 // TODO

  def pos_startColumnPadding(pos: SourcePosition)  = ""            // TODO
  def pos_beforeAndAfterPoint(pos: SourcePosition) = (Nil, Nil)    // TODO
  def pos_linesSlice(pos: SourcePosition)          = Array[Char]() // TODO

  def pageWidth = 80
  def EOL       = System.lineSeparator
  final val LF  = '\u000A'
  final val FF  = '\u000C'
  final val CR  = '\u000D'
  final val SU  = '\u001A'

  def absFileExists(absFile: dotc.interfaces.AbstractFile): Boolean = {
    absFile.jfile.isPresent || Files.exists(Paths.get(absFile.path))
  }
}

final class SimpleStoreReporter extends SimpleReporter {
  val infos: ListBuffer[Diagnostic] = new ListBuffer
  var   errorCount = 0
  var warningCount = 0

  def report(dia: Diagnostic): Unit = {
    infos += dia
    dia.level match {
      case Diagnostic.ERROR   =>   errorCount += 1
      case Diagnostic.WARNING => warningCount += 1
      case Diagnostic.INFO    =>
    }
  }

  def hasErrors: Boolean = errorCount > 0
}

trait Invoke {
  def id: String
  def compile1(src: Path, out: Path): CompileResult
}

object Scala3Main extends shisa.MainClass {
  val combinations = Seq[Invoke](
    FreshCompiler3("3.0-old",  List("-source", "3.0-migration")),
    FreshCompiler3("3.0",      Nil), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr", List("-source", "3.1-migration")),
    FreshCompiler3("3.1",      List("-source", "3.1")),
  )
}

final case class FreshCompiler3(id: String, options: List[String]) extends Invoke {
  def compile1(src: Path, out: Path): CompileResult = {
    val args = List(
      List("-color:never", "-explain", "-migration"),
      List("-classpath", Deps.scalac_3_00_base.mkString(File.pathSeparator)),
      List("-d", out.toString),
      List("-usejavacp"),
      //List("-Ydrop-comments"), // "Trying to pickle comments, but there's no `docCtx`."
    ).flatten ::: options ::: List(src.toString)
    val reporter  = new SimpleStoreReporter
    val mainClass = Class.forName("dotty.tools.dotc.Main")
    val process = mainClass.getMethod("process",
      classOf[Array[String]], classOf[SimpleReporter], classOf[dotc.interfaces.CompilerCallback])
    process.invoke(null, args.toArray, reporter, /* callback = */ null)
    CompileResult(if (reporter.hasErrors) 1 else 0, reporter.infos.toList.map(InvokeCompiler.display))
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
