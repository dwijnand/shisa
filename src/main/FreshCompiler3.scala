package shisa

import java.io.File
import java.nio.file.Path

import dotty.tools.dotc, dotc._, core.Contexts._, reporting._

object ShisaDriver extends Driver {
  override def doCompile(compiler: Compiler, fileNames: List[String])(using Context): Reporter =
    super.doCompile(compiler, fileNames)
}

final case class FreshCompiler3(id: String, scalaJars: Seq[File], cmd: String) extends Invoke {
  def mkRunner(): Runner = new Runner {
    implicit val ctx: FreshContext = new ContextBase().initialCtx.fresh
    ctx.setSetting(ctx.settings.color, "never")
    ctx.setSetting(ctx.settings.classpath, scalaJars.mkString(File.pathSeparator))
    ctx.setSetting(ctx.settings.explain, true)
    ctx.setSetting(ctx.settings.migration, true)
    ctx.setSetting(ctx.settings.outputDir, new dotty.tools.io.VirtualDirectory(s"FreshCompiler3 output"))
    ctx.setSetting(ctx.settings.YdropComments, true) // "Trying to pickle comments, but there's no `docCtx`."
    ctx.setSettings(ctx.settings.processArguments(config.CommandLineParser.tokenize(cmd), processAll = true).sstate)
    ast.Positioned.updateDebugPos(using ctx)
    val compiler = new dotty.tools.dotc.Compiler

    def compile1(src: Path) = {
      val reporter = new StoreReporter(outer = null) with UniqueMessagePositions with HideNonSensicalMessages
      ctx.setReporter(reporter)
      ShisaDriver.doCompile(compiler, List(src.toString))
      new CompileResult(if (reporter.hasErrors) 1 else 0, reporter.removeBufferedMessages.toList.map(display))
    }
  }

  // from dotc.reporting.ConsoleReporter
  def display(dia: Diagnostic)(implicit ctx: Context): String = {
    import Diagnostic._
    val doIt = dia match {
      case dia: ConditionalWarning => dia.enablingOption.value
      case _                       => true
    }

    if (doIt) {
      val rendering = new MessageRendering {}; import rendering._
      val builder   = new StringBuilder("")
      def addMsg    = builder ++= (_: String)

      addMsg(messageAndPos(dia.msg, dia.pos, diagnosticLevel(dia)))

      if (shouldExplain(dia))
        addMsg("\n" + explanation(dia.msg))
      else if (dia.msg.explanation.nonEmpty)
        addMsg("\nlonger explanation available when compiling with `-explain`")

      builder.result()
    } else ""
  }
}
