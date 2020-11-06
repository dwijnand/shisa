package shisa

import java.io.File
import java.nio.file.Path

import scala.jdk.CollectionConverters._

import dotty.tools.dotc
import dotc.{ Run, Compiler => _ }
import dotc.ast.Positioned
import dotc.config.CommandLineParser
import dotc.core.Contexts._
import dotc.reporting._
import dotc.util.SourceFile
import dotty.tools.io.VirtualDirectory

final case class FreshCompiler3(id: String, scalaJars: Array[File], cmd: String) extends MkCompiler {
  def mkCompiler(): Compiler = new Compiler {
    val id  = FreshCompiler3.this.id
    val cmd = FreshCompiler3.this.cmd

    implicit val ctx: FreshContext = new ContextBase().initialCtx.fresh
    ctx.setSetting(ctx.settings.color, "never")
    ctx.setSetting(ctx.settings.classpath, scalaJars.mkString(File.pathSeparator))
    ctx.setSetting(ctx.settings.explain, true)
    ctx.setSetting(ctx.settings.migration, true)
    ctx.setSetting(ctx.settings.outputDir, new VirtualDirectory("FreshCompiler3 output", /* maybeContainer = */ None))
    ctx.setSetting(ctx.settings.YdropComments, true) // "Trying to pickle comments, but there's no `docCtx`."
    ctx.setSettings(ctx.settings.processArguments(CommandLineParser.tokenize(cmd), /* processAll = */ true).sstate)
    Positioned.updateDebugPos
    val compiler = new dotc.Compiler

    def compile1(src: Path) = {
      ctx.setReporter(new StoreReporter(/* outer = */ null) with UniqueMessagePositions with HideNonSensicalMessages)
      val source = SourceFile.virtual(src.getFileName.toString, java.nio.file.Files.readString(src))
      val run: Run = compiler.newRun
      run.compileSources(List(source))
      assert(ctx.reporter.errorsReported || run.suspendedUnits.isEmpty, "Suspended units support now required")
      new Msgs(ctx.reporter.removeBufferedMessages.map(getMsg(_)).asJava)
    }
  }

  def getMsg(dia: Diagnostic)(implicit ctx: Context) = new Msg(getSeverity(dia), dia.pos.nonInlined.line + 1, dia.message)

  def getSeverity(dia: Diagnostic) = dia match {
    case _: Diagnostic.Error              => Severity.Error
    case _: Diagnostic.FeatureWarning     => Severity.Warn
    case _: Diagnostic.DeprecationWarning => Severity.Warn
    case _: Diagnostic.UncheckedWarning   => Severity.Warn
    case _: Diagnostic.MigrationWarning   => Severity.Warn
    case _: Diagnostic.Warning            => Severity.Warn
    case _: Diagnostic.Info               => Severity.Info
  }
}
