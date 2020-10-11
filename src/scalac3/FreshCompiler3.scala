package shisa

import java.io.File
import java.nio.file.Path

import scala.jdk.CollectionConverters._

import dotty.tools.dotc, dotc.{ Compiler => _, _ }, ast.Positioned, config.CommandLineParser, core.Contexts._, reporting._
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
    ctx.setSetting(ctx.settings.outputDir, new VirtualDirectory("FreshCompiler3 output"))
    ctx.setSetting(ctx.settings.YdropComments, true) // "Trying to pickle comments, but there's no `docCtx`."
    ctx.setSettings(ctx.settings.processArguments(CommandLineParser.tokenize(cmd), processAll = true).sstate)
    Positioned.updateDebugPos
    val compiler = new dotc.Compiler

    def compile1(src: Path) = {
      ctx.setReporter(new StoreReporter(outer = null) with UniqueMessagePositions with HideNonSensicalMessages)
      FreshCompiler3.Driver.doCompile(compiler, List(src.toString))
      val lines = ctx.reporter.removeBufferedMessages.map(FreshCompiler3.display)
      new CompileResult(ctx.reporter.hasErrors, lines.asJava)
    }
  }
}

object FreshCompiler3 {
  // from dotc.reporting.ConsoleReporter
  def display(dia: Diagnostic)(implicit ctx: Context): String = {
    val doIt = dia match {
      case dia: Diagnostic.ConditionalWarning => dia.enablingOption.value
      case _                                  => true
    }
    if (doIt) display1(dia) else ""
  }

  private object rendering extends MessageRendering
  import rendering._

  private def display1(dia: Diagnostic)(implicit ctx: Context): String = {
    val b = new StringBuilder(messageAndPos(dia.msg, dia.pos, diagnosticLevel(dia)))

    if (Diagnostic.shouldExplain(dia))
      b ++= ("\n" + explanation(dia.msg))
    else if (dia.msg.explanation.nonEmpty)
      b ++= ("\nlonger explanation available when compiling with `-explain`")

    b.result()
  }

  object Driver extends dotc.Driver {
    override def doCompile(compiler: dotc.Compiler, fileNames: List[String])(using Context): Reporter =
        super.doCompile(compiler, fileNames)
  }
}
