package shisa

import java.io.File
import java.nio.file.Path

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import dotty.tools.{ FatalError, dotc }
import dotc.{ Driver, Run, report, Compiler => _ }
import dotc.ast.Positioned
import dotc.config.{ CommandLineParser, Settings }
import dotc.core.TypeError
import dotc.core.Contexts._
import dotc.reporting._
import dotc.util.NoSourcePosition
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
      Scalac3Driver.doCompile(compiler, List(src.toString))
      new Msgs(ctx.reporter.removeBufferedMessages.map(FreshCompiler3.diaToMsg(_)).asJava)
    }
  }
}

object FreshCompiler3 {
  def diaToMsg(dia: Diagnostic)(implicit ctx: Context): Msg = {
    val pos = dia.pos.nonInlined
    new Msg(diaSeverity(dia), pos.line + 1, dia.message)
  }

  def diaSeverity(dia: Diagnostic) = dia match {
    case _: Diagnostic.Error              => Severity.Error
    case _: Diagnostic.FeatureWarning     => Severity.Warn
    case _: Diagnostic.DeprecationWarning => Severity.Warn
    case _: Diagnostic.UncheckedWarning   => Severity.Warn
    case _: Diagnostic.MigrationWarning   => Severity.Warn
    case _: Diagnostic.Warning            => Severity.Warn
    case _: Diagnostic.Info               => Severity.Info
  }
}

object Scalac3Driver extends Driver {
  override def doCompile(compiler: dotc.Compiler, fileNames: List[String])(implicit ctx: Context): Reporter = {
    if (fileNames.nonEmpty)
      try {
        val run = compiler.newRun
        run.compile(fileNames)

        @tailrec def finish(run: Run): Unit = {
          run.printSummary()
          if (!ctx.reporter.errorsReported && run.suspendedUnits.nonEmpty) {
            val suspendedUnits = run.suspendedUnits.toList
            if (ctx.settings.XprintSuspension.value)
              report.echo(s"compiling suspended ${suspendedUnits.mkString(", ")}", NoSourcePosition)
            val run1 = compiler.newRun
            for (unit <- suspendedUnits)
              unit.suspended = false
            run1.compileUnits(suspendedUnits)
            finish(run1)
          }
        }

        finish(run)
      } catch {
        case ex: FatalError  =>
          report.error(new NoExplanation(ex.getMessage), NoSourcePosition, /* sticky = */ false) // signals that we should fail compilation.
        case ex: TypeError =>
          println(s"${ex.toMessage} while compiling ${fileNames.mkString(", ")}")
          throw ex
        case ex: Throwable =>
          println(s"$ex while compiling ${fileNames.mkString(", ")}")
          throw ex
      }
    ctx.reporter
  }
}
