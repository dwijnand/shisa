package shisa

import java.io.File

import scala.reflect.internal
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc, nsc._, reporters.StoreReporter

final case class FreshCompiler2(id: String, scalaJars: Seq[File], cmd: String) extends MkCompiler { self =>
  val settings = new Settings
  settings.classpath.value = scalaJars.mkString(File.pathSeparator)
  settings.deprecation.value = true
  settings.outputDirs.setSingleOutput(new VirtualDirectory("", None))
  settings.processArgumentString(cmd) match {
    case (true, Nil)            =>
    case (success, unprocessed) => sys.error(s"FreshCompiler2 id=$id success=$success unprocessed=$unprocessed")
  }

  def mkCompiler: Compiler = new Compiler {
    val reporter = new StoreReporter(settings)
    val compiler = Global(settings, reporter)

    def compile1(src: SrcFile) = try {
      new compiler.Run().compileSources(List(compiler.newSourceFile(src.content, src.name)))
      reporter.infos.toList.map(info => Msg(getSev(info), info.msg))
    } finally reporter.reset()
  }

  def getSev(info: StoreReporter.Info) = info.severity match {
    case internal.Reporter.ERROR   => E
    case internal.Reporter.WARNING => W
    case internal.Reporter.INFO    => throw new Exception("Unexpected info msg")
  }
}
