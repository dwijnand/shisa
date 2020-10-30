package shisa

import java.io.File
import java.nio.file.Path

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.{ Position, StringOps }
import scala.reflect.internal.{ Reporter => IReporter }
import scala.reflect.io.{ AbstractFile, VirtualDirectory }
import scala.tools.nsc, nsc._, reporters.{ Reporter, StoreReporter }

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
    new Msg(infoSeverity(info), info.pos.source.file.path, info.pos.line, info.msg, display(info))
  }

  def infoSeverity(info: StoreReporter.Info): Severity = info.severity match {
    case IReporter.ERROR   => Severity.Error
    case IReporter.WARNING => Severity.Warning
    case IReporter.INFO    => Severity.Info
  }

  // from nsc.reporters.PrintReporter
  def display(info: StoreReporter.Info): String = {
    val label = info.severity match {
      case IReporter.ERROR   => "error: "
      case IReporter.WARNING => "warning: "
      case IReporter.INFO    => ""
    }
    val msg  = Reporter.explanation(info.msg)
    val text = Position.formatMessage(info.pos, s"$label$msg", shortenFile = false)
    StringOps.trimAllTrailingSpace(text)
    // target/testdata/Call.##.scala:8: error: Int does not take parameters
    //   any.##()
    //         ^
  }

  // from nsc.reporters.ConsoleReporter
  def finish(reporter: IReporter): Unit = {
    def echo(lbl: String, n: Int) = if (n > 0) reporter.echo(StringOps.countElementsAsString(n, lbl))
    echo("warning", reporter.warningCount)
    echo("error", reporter.errorCount)
  }
}
