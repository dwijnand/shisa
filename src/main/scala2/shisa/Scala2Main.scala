package shisa

import java.io.File
import java.nio.file.Path

import scala.reflect.internal.util.{ Position, StringOps }
import scala.reflect.internal.Reporter.{ ERROR, WARNING }
import scala.reflect.io.AbstractFile
import scala.tools.nsc, nsc._, reporters.StoreReporter

import coursier._

object Scala2Main extends shisa.MainClass {
  def main(args: Array[String]): Unit = runArgs(args.toList)

  val dq            = '"'
  val getVersion    = () => Exec.execStr("scala -2.13.head -e " +
      s"'println(scala.util.Properties.scalaPropOrNone(${dq}maven.version.number$dq).get)'")
  val getVersionErr =
    (res: Exec.Result) => sys.error(s"Fail: ${res.exitCode}, lines:\n  ${res.lines.mkString("\n  ")}")

  def getVersionOr(alt: PartialFunction[Exec.Result, String]) = getVersion() match {
    case Exec.Result(0, List(s)) => s
    case res                     => alt.applyOrElse(res, getVersionErr)
  }

  // if we get extra lines from Coursier, run again
  lazy val _2_13_head = getVersionOr { case Exec.Result(0, _) => getVersionOr(PartialFunction.empty) }

  def scalaEaRepo = MavenRepository("https://scala-ci.typesafe.com/artifactory/scala-integration")
  def scalaPrRepo = MavenRepository("https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots")

  def lib(v: String) = Dependency(mod"org.scala-lang:scala-library", v)
  lazy val baseJars = Fetch().addDependencies(lib("2.13.3")).run()
  lazy val headJars = Fetch().addDependencies(lib(_2_13_head)).addRepositories(scalaEaRepo).run()

  override lazy val combinations = Seq[Invoke](
    FreshCompiler2("2.13-base", baseJars, ""),
    FreshCompiler2("2.13-head", headJars, ""),
    FreshCompiler2("2.13-new",  headJars, "-Xsource:3"),
  )
}

final case class FreshCompiler2(id: String, scalaJars: Seq[File], cmd: String) extends Invoke {
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
