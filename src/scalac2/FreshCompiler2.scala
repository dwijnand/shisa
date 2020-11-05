package shisa

import java.io.File
import java.nio.file.Path

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.StringOps
import scala.reflect.internal
import scala.reflect.io.{ AbstractFile, VirtualDirectory }
import scala.tools.nsc, nsc._, reporters.StoreReporter

final case class FreshCompiler2(id: String, scalaJars: Seq[File], cmd: String) extends MkCompiler {
  def mkCompiler(): Compiler = new Compiler {
    val id  = FreshCompiler2.this.id
    val cmd = FreshCompiler2.this.cmd

    val settings = new Settings
    settings.classpath.value = scalaJars.mkString(File.pathSeparator)
    settings.deprecation.value = true
    settings.outputDirs.setSingleOutput(new VirtualDirectory("FreshCompiler2 output", None))
    settings.processArgumentString(cmd)
    val reporter = new StoreReporter(settings)
    val compiler = Global(settings, reporter)

    def compile1(src: Path) = try {
      new compiler.Run().compileFiles(List(AbstractFile.getFile(src.toFile)))
      FreshCompiler2.finish(reporter)
      val msgs = reporter.infos.toList.map(FreshCompiler2.infoToMsg(_))
      new Msgs(msgs.asJava)
    } finally reporter.reset()
  }
}

object FreshCompiler2 {
  def infoToMsg(info: StoreReporter.Info): Msg = {
    new Msg(infoSeverity(info), info.pos.line, info.msg)
  }

  def infoSeverity(info: StoreReporter.Info): Severity = info.severity match {
    case internal.Reporter.ERROR   => Severity.Error
    case internal.Reporter.WARNING => Severity.Warn
    case internal.Reporter.INFO    => Severity.Info
  }

  // from nsc.reporters.ConsoleReporter
  def finish(reporter: internal.Reporter): Unit = {
    def echo(lbl: String, n: Int) = if (n > 0) reporter.echo(StringOps.countElementsAsString(n, lbl))
    echo("warning", reporter.warningCount)
    echo("error",   reporter.errorCount)
  }
}
