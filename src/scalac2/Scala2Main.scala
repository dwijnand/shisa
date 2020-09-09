package shisa

import java.io.File
import java.nio.file.Path

import scala.reflect.internal.util.{ Position, StringOps }
import scala.reflect.internal.Reporter.{ ERROR, WARNING }
import scala.reflect.io.AbstractFile
import scala.tools.nsc, nsc._, reporters.StoreReporter

final case class FreshCompiler2(id: String, scalaJars: Seq[File], cmd: String) extends Invoke {
  def compile1(src: Path, out: Path): CompileResult = {
    val settings = new Settings
    settings.classpath.value   = scalaJars.mkString(File.pathSeparator)
    settings.deprecation.value = true
    settings.outdir.value      = out.toString
    settings.processArgumentString(cmd)
    val reporter = new StoreReporter(settings)
    val compiler = Global(settings, reporter)
    new compiler.Run().compileFiles(List(AbstractFile.getFile(src.toFile)))
    finish(reporter)
    CompileResult(if (reporter.hasErrors) 1 else 0, reporter.infos.toList.map(display))
  }

  // from nsc.reporters.PrintReporter
  def display(info: StoreReporter.Info): String = {
    val label = info.severity match {
      case ERROR   => "error: "
      case WARNING => "warning: "
      case _       => ""
    }
    val msg  = reporters.Reporter.explanation(info.msg)
    val text = Position.formatMessage(info.pos, s"$label$msg", shortenFile = false)
    StringOps.trimAllTrailingSpace(text)
  }

  // from nsc.reporters.ConsoleReporter
  def finish(reporter: StoreReporter): Unit = {
    def echo(lbl: String, n: Int) = if (n > 0) reporter.echo(StringOps.countElementsAsString(n, lbl))
    echo("warning", if (reporter.settings.nowarn) 0 else reporter.warningCount)
    echo("error", reporter.errorCount)
    reporter.finish()
  }
}
