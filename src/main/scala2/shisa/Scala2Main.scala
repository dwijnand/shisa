package shisa

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Path

import scala.reflect.internal.util.{ Position, StringOps }
import scala.reflect.internal.Reporter.{ ERROR, WARNING }
import scala.reflect.io.AbstractFile
import scala.tools.nsc, nsc._, reporters.StoreReporter
import scala.util.Try

object Scala2Main extends shisa.MainClass {
  val combinations = Seq[Invoke](
    FreshCompiler2("2.13-base", Deps.lib_2_13_base, ""),
    FreshCompiler2("2.13-head", Deps.lib_2_13_head, ""),
    FreshCompiler2("2.13-new",  Deps.lib_2_13_head, "-Xsource:3"),
  )
}

final case class FreshCompiler2(id: String, scalaJars: Seq[File], cmd: String) extends Invoke {
  lazy val classLoader = new URLClassLoader(scalaJars.toArray.map(_.toURI.toURL), parentClassLoader())

  def parentClassLoader() = Try { // needed for nsc 2.11 on JDK 11
    classOf[ClassLoader].getMethod("getPlatformClassLoader").invoke(null).asInstanceOf[ClassLoader]
  }.getOrElse(null) // on JDK 8 javaBootClassPath works

  def compile(args: Seq[String]): Boolean = {
    import scala.language.reflectiveCalls
    val cls = classLoader.loadClass("scala.tools.nsc.Main$")
    type Driver   = { def process(args: Array[String]): Any; def reporter: Reporter }
    type Reporter = { def hasErrors: Boolean }
    val driver = Deps.getObj[Driver](cls)
    driver.process(args.toArray) match {
      case b: Boolean => b
      case null       => !driver.reporter.hasErrors // nsc 2.11
    }
  }

  def compile1(src: Path, out: Path): CompileResult = {
    val settings = new Settings
    settings.classpath.value   = scalaJars.mkString(File.pathSeparator)
    settings.deprecation.value = true
    settings.outdir.value      = out.toString
    settings.processArgumentString(cmd)
    val reporter = new StoreReporter(settings)
    val compiler = Global(settings, reporter)
    new compiler.Run().compileFiles(List(AbstractFile.getFile(src.toFile)))
    finish(reporter)
    CompileResult(if (reporter.hasErrors) 1 else 0, reporter.infos.toList.map(display))
  }

  // from nsc.reporters.PrintReporter
  def display(info: StoreReporter.Info): String = {
    val label = info.severity match {
      case ERROR   => "error: "
      case WARNING => "warning: "
      case _       => ""
    }
    val msg  = reporters.Reporter.explanation(info.msg)
    val text = Position.formatMessage(info.pos, s"$label$msg", shortenFile = false)
    StringOps.trimAllTrailingSpace(text)
  }

  // from nsc.reporters.ConsoleReporter
  def finish(reporter: StoreReporter): Unit = {
    def echo(lbl: String, n: Int) = if (n > 0) reporter.echo(StringOps.countElementsAsString(n, lbl))
    echo("warning", if (reporter.settings.nowarn) 0 else reporter.warningCount)
    echo("error", reporter.errorCount)
    reporter.finish()
  }
}
