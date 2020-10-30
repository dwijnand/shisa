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
    new Msg(diaSeverity(dia), pos.source.file.path, pos.line + 1, dia.message, FreshCompiler3.display(dia))
  }

  def diaSeverity(dia: Diagnostic) = dia match {
    case _: Diagnostic.Error              => Severity.Error
    case _: Diagnostic.FeatureWarning     => Severity.Warning
    case _: Diagnostic.DeprecationWarning => Severity.Warning
    case _: Diagnostic.UncheckedWarning   => Severity.Warning
    case _: Diagnostic.MigrationWarning   => Severity.Warning
    case _: Diagnostic.Warning            => Severity.Warning
    case _: Diagnostic.Info               => Severity.Info
  }

  // from dotc.reporting.ConsoleReporter
  def display(dia: Diagnostic)(implicit ctx: Context): String = dia match {
    case dia: Diagnostic.ConditionalWarning => if (dia.enablingOption.value) display1(dia) else ""
    case _                                  => display1(dia)
  }

  private def display1(dia: Diagnostic)(implicit ctx: Context): String = {
    // Error/Warning/Info/Feature Warning/Deprecation Warning/Unchecked Warning/Migration Warning
    val diaLvl  = rendering.diagnosticLevel(dia)
    val msgPos  = rendering.messageAndPos(dia.msg, dia.pos, diaLvl)
    val explain = if (dia.msg.explanation.isEmpty) "" else "\n" + rendering.explanation(dia.msg)
    msgPos + explain
    // -- [E050] Type Error: target/testdata/Call.##.scala:8:6 ------------------------
    // 8 |  any.##()
    //   |  ^^^^^^
    //   |  method ## in class Any does not take parameters
    //
    // Explanation
    // ===========
    // You have specified more parameter lists as defined in the method definition(s).
    // Nullary methods may not be called with parenthesis
  }
  // AbstractFile { name: String, path: String, jfile: Optional[File] }
  // SourceFile <: AbstractFile { content: Array[Char] }
  // SourcePosition {
  //          source: SourceFile,
  //     lineContent: String,
  //     point,      line,      column: Int,
  //     start, startLine, startColumn: Int,
  //       end,   endLine,   endColumn: Int,
  // }
  // Diagnostic (level): Error Warning Info FeatureWarning DeprecationWarning UncheckedWarning MigrationWarning

  object rendering extends MessageRendering
  object Driver extends dotc.Driver {
    override def doCompile(compiler: dotc.Compiler, fileNames: List[String])(using Context): Reporter =
        super.doCompile(compiler, fileNames)
  }
}
