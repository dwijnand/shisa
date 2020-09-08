package shisa

import java.io.File
import java.nio.file.Path

import dotty.tools.dotc, dotc._, core.Contexts._, reporting._

import coursier._

object Scala3Main extends shisa.MainClass {
  val combinations = Seq[Invoke](
    FreshCompiler3("3.0-old",  "-source 3.0-migration"),
    FreshCompiler3("3.0",      ""), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr", "-source 3.1-migration"),
    FreshCompiler3("3.1",      "-source 3.1"),
  )
}

object ShisaDriver extends Driver {
  override def doCompile(compiler: Compiler, fileNames: List[String])(using Context): Reporter =
    super.doCompile(compiler, fileNames)
}

final case class FreshCompiler3(id: String, cmd: String) extends Invoke {
  def compile1(src: Path, out: Path): CompileResult = {
    implicit val ctx: FreshContext = new ContextBase().initialCtx.fresh
    val reporter = new StoreReporter(outer = null) with UniqueMessagePositions with HideNonSensicalMessages
    ctx.setReporter(reporter)
    ctx.setSetting(ctx.settings.color, "never")
    ctx.setSetting(ctx.settings.classpath, Deps.scalac_3_00_base.mkString(File.pathSeparator))
    ctx.setSetting(ctx.settings.explain, true)
    ctx.setSetting(ctx.settings.migration, true)
    ctx.setSetting(ctx.settings.outputDir, dotty.tools.io.AbstractFile.getDirectory(out.toAbsolutePath.toString))
    ctx.setSetting(ctx.settings.YdropComments, true) // "Trying to pickle comments, but there's no `docCtx`."
    ctx.setSettings(ctx.settings.processArguments(config.CommandLineParser.tokenize(cmd), processAll = true).sstate)
    ast.Positioned.updateDebugPos(using ctx)
    ShisaDriver.doCompile(new dotty.tools.dotc.Compiler, List(src.toString))
    CompileResult(if (reporter.hasErrors) 1 else 0, reporter.removeBufferedMessages.toList.map(display))
  }

  // from dotc.reporting.ConsoleReporter
  def display(dia: Diagnostic)(using Context): String = {
    import Diagnostic._
    val doIt = dia match {
      case dia: ConditionalWarning => dia.enablingOption.value
      case _                       => true
    }

    if (doIt) {
      val rendering = new MessageRendering {}; import rendering._
      val builder   = new StringBuilder("")
      def addMsg    = builder ++= (_: String)

      if (doIt) addMsg(messageAndPos(dia.msg, dia.pos, diagnosticLevel(dia)))
      if (doIt && shouldExplain(dia)) addMsg("\n" + explanation(dia.msg))
      else if (doIt && dia.msg.explanation.nonEmpty) addMsg("\nlonger explanation available when compiling with `-explain`")

      builder.result()
    } else ""
  }
}
