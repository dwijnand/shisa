package shisa

import java.io.File

import scala.jdk.CollectionConverters._
import scala.reflect.internal
import scala.reflect.io.VirtualDirectory
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

    def compile1(src: SrcFile) = try {
      new compiler.Run().compileSources(List(compiler.newSourceFile(src.content, src.name)))
      new Msgs(reporter.infos.toList.map(getMsg(_)).asJava)
    } finally reporter.reset()
  }

  def getMsg(info: StoreReporter.Info) = new Msg(getSeverity(info), info.pos.line, info.msg)

  def getSeverity(info: StoreReporter.Info) = info.severity match {
    case internal.Reporter.ERROR   => Severity.Error
    case internal.Reporter.WARNING => Severity.Warn
    case internal.Reporter.INFO    => Severity.Info
  }
}
