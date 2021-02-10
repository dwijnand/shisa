package shisa

import java.io.File

import dotty.tools.dotc
import dotc.{ Run, Compiler => _ }
import dotc.ast.Positioned
import dotc.config.CommandLineParser, dotc.config.Settings.ArgsSummary
import dotc.core.Contexts._
import dotc.reporting._
import dotc.util.SourceFile
import dotty.tools.io.VirtualDirectory

final case class FreshCompiler3(id: String, scalaJars: Seq[File], cmd: String) extends MkCompiler { self =>
  def mkCompiler: Compiler = new Compiler {
    implicit val ctx: FreshContext = new ContextBase().initialCtx.fresh
    ctx.setSetting(ctx.settings.color, "never")
    ctx.setSetting(ctx.settings.classpath, scalaJars.mkString(File.pathSeparator))
    ctx.setSetting(ctx.settings.explain, true)
    ctx.setSetting(ctx.settings.outputDir, new VirtualDirectory("", /* maybeContainer = */ None))
    ctx.setSetting(ctx.settings.YdropComments, true) // "Trying to pickle comments, but there's no `docCtx`."
    val settings = ctx.settings.processArguments(CommandLineParser.tokenize(cmd), /* processAll = */ true) match {
      case ArgsSummary(settings, _, Nil, _)    => settings
      case ArgsSummary(_, _, errors, warnings) => sys.error(s"FreshCompiler3 id=$id failed errors=$errors warnings=$warnings")
    }
    ctx.setSettings(settings)
    Positioned.init
    val compiler = new dotc.Compiler

    def compile1(src: SrcFile) = {
      ctx.setReporter(new StoreReporter(/* outer = */ null) with UniqueMessagePositions with HideNonSensicalMessages)
      val run: Run = compiler.newRun
      run.compileSources(List(SourceFile.virtual(src.name, src.content)))
      assert(ctx.reporter.errorsReported || run.suspendedUnits.isEmpty, "Suspended units support now required")
      val dias: List[Diagnostic] = ctx.reporter.removeBufferedMessages
      dias.map(dia => Msg(getSev(dia), dia.message))
    }
  }

  def getSev(dia: Diagnostic) = dia match {
    case _: Diagnostic.Error              => E
    case _: Diagnostic.FeatureWarning     => W
    case _: Diagnostic.DeprecationWarning => W
    case _: Diagnostic.UncheckedWarning   => W
    case _: Diagnostic.MigrationWarning   => W
    case _: Diagnostic.Warning            => W
    case _: Diagnostic.Info               => throw new Exception("Unexpected info msg")
  }
}
