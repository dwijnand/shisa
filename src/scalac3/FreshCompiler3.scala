package shisa

import java.io.File
import java.nio.file.Path

import scala.jdk.CollectionConverters._
import dotty.tools.dotc
import dotc.{ Compiler => _, _ }
import ast.Positioned
import config.CommandLineParser
import core.Contexts._
import reporting._
import dotty.tools.io.VirtualDirectory
import shisa.FreshCompiler3.diaToMsg

final case class FreshCompiler3(id: String, scalaJars: Array[File], cmd: String) extends MkCompiler {
  def mkCompiler(): Compiler = new Compiler {
    val id  = FreshCompiler3.this.id
    val cmd = FreshCompiler3.this.cmd

    implicit val ctx: FreshContext = new ContextBase().initialCtx.fresh
    ctx.setSetting(ctx.settings.color, "never")
    ctx.setSetting(ctx.settings.classpath, scalaJars.mkString(File.pathSeparator))
    ctx.setSetting(ctx.settings.explain, true)
    ctx.setSetting(ctx.settings.migration, true)
    ctx.setSetting(ctx.settings.outputDir, new VirtualDirectory("FreshCompiler3 output"))
    ctx.setSetting(ctx.settings.YdropComments, true) // "Trying to pickle comments, but there's no `docCtx`."
    ctx.setSettings(ctx.settings.processArguments(CommandLineParser.tokenize(cmd), processAll = true).sstate)
    Positioned.updateDebugPos
    val compiler = new dotc.Compiler

    def compile1(src: Path) = {
      ctx.setReporter(new StoreReporter(outer = null) with UniqueMessagePositions with HideNonSensicalMessages)
      FreshCompiler3.Driver.doCompile(compiler, List(src.toString))
      new Msgs(ctx.reporter.removeBufferedMessages.map(diaToMsg(_)).asJava)
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

  object Driver extends dotc.Driver {
    override def doCompile(compiler: dotc.Compiler, fileNames: List[String])(using Context): Reporter =
        super.doCompile(compiler, fileNames)
  }
}
