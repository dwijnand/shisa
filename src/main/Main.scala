package shisa

import scala.language.implicitConversions

import java.io.File
import java.net.URLClassLoader
import java.nio.file._
import java.util.concurrent.Executors

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

import scala.meta._

import shisa.testdata._

object Main {
  val dotcCp = BuildInfo.scalac3Dir +: Deps.scalac_3_00_base
  val dotcCl = new URLClassLoader(dotcCp.map(_.toURI.toURL).toArray, getClass.getClassLoader)
  val freshCompiler3Cls  = dotcCl.loadClass("shisa.FreshCompiler3")
  val freshCompiler3Ctor = freshCompiler3Cls.getConstructor(classOf[String], classOf[Array[File]], classOf[String])

  def FreshCompiler3(id: String, cmd: String): MkCompiler =
    freshCompiler3Ctor.newInstance(id, Deps.scalac_3_00_base.toArray, cmd).asInstanceOf[MkCompiler]

  val mkCompilers = List[MkCompiler](
    FreshCompiler2("2.13-base", Deps.scalac_2_13_base, ""),
    FreshCompiler2("2.13-head", Deps.scalac_2_13_head, ""),
    FreshCompiler2("2.13-new",  Deps.scalac_2_13_head, "-Xsource:3"),
    FreshCompiler3("3.0-old",                          "-source 3.0-migration"),
    FreshCompiler3("3.0",                              ""), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr",                         "-source 3.1-migration"),
    FreshCompiler3("3.1",                              "-source 3.1"),
  )

  val compilerIds   = mkCompilers.map(_.id)
  val tests         = Call.tests ::: EtaX.tests
  val MissingExp    = new Msg(Severity.Error, 0, "missing exp msg")
  val MissingObt    = new Msg(Severity.Error, 0, "missing obt msg")

  def idxStr(idx: Int) = if (idx < 10) s"0$idx" else s"$idx"

  def main(args: Array[String]): Unit = {
    val pool    = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val futures = tests.map(tc => pool.submit[TestResult](() => compile1(tc, mkCompilers)))

    pool.shutdown()

    if (!pool.awaitTermination(10, MINUTES))
      throw new Exception("Thread pool timeout elapsed before all tests were complete!")

    val testResults  = futures.map(_.get(0, NANOSECONDS))
    val testFailures = testResults.collect {
      case TestFailure(msg)       => s"$msg"
      case TestFailures(failures) => s"\n${failures.map(tf => s"  ${tf.msg}").mkString("\n")}"
    }

    if (testFailures.nonEmpty) {
      System.err.println(s"Test failures:\n${testFailures.mkString("\n")}")
      throw new Exception(s"Test failures", null, false, false) {}
    }
  }

  def compile1(testContents: TestContents, mkCompilers: List[MkCompiler]): TestResult = {
    val compilers = mkCompilers.map(_.mkCompiler())
    if (testContents.stats.sizeIs > 1) doLines(testContents, compilers)
    else                                doUnit(testContents, compilers)
  }

  def doUnit(testContents: TestContents, compilers: List[Compiler]): TestResult = {
    compareMsgs(testContents, writeAndCompile(compilers, toSource(testContents)))
  }

  def doLines(testContents: TestContents, compilers: List[Compiler]) = {
    val msgss = testContents.stats
      .map(stat => testContents.copy(stats = List(stat)))
      .zipWithIndex.map { case (contents, idx) =>
        writeAndCompile(compilers, toSource(contents, Some(idx)))
      }.reduce(_.zip(_).map { case (a, b) => a ::: b })
    compareMsgs(testContents, msgss)
  }

  def toSource(contents: TestContents, pkgIdx: Option[Int] = None): String = {
    val sourceDefn = q"object Test { ..${contents.defns ::: contents.stats.flatten} }"
    val source     = pkgIdx.map(idx => Term.Name(s"p${idxStr(idx)}")) match {
      case None          => source"$sourceDefn"
      case Some(pkgName) => source"package $pkgName; $sourceDefn"
    }
    source.syntax + "\n"
  }

  def writeAndCompile(compilers: List[Compiler], sourceStr: String) = {
    val src = Files.createTempFile("shisa", ".scala")
    Files.writeString(src, sourceStr)
    compilers.map(_.compile1(src)).map(msgsDropSummary)
  }

  def compareMsgs(contents: TestContents, obtMsgss: List[List[Msg]]): TestResult = {
    val TestContents(_, _, expMsgss) = contents
    val msgss2 = expMsgss.zipAll(obtMsgss, Nil, Nil)
    val msgss3 = msgss2.zipAll(compilerIds, (Nil, Nil), "<unknown-compiler-id>")
    val testResults = for (((expMsgs, obtMsgs), compilerId) <- msgss3) yield {
      expMsgs.zipAll(obtMsgs, MissingExp, MissingObt).collect {
        case (exp, obt) if exp != obt => showExp(exp) + showObt(obt)
      }.mkString match {
        case ""    => TestSuccess
        case lines => TestFailure(s"message mismatch (compiler $compilerId) (${Console.RED}-expected${Console.RESET}/${Console.GREEN}+obtained${Console.RESET}):$lines")
      }
    }
    testResults.collect { case tf: TestFailure => tf } match {
      case Nil          => TestSuccess
      case testFailures => TestFailures(testFailures)
    }
  }

  // drop summary ("3 errors"/"3 errors found")
  def msgsDropSummary(msgs: Msgs) = msgs.msgs.asScala.toList.takeWhile(_.lineNo != 0)

  val LineStart         = "(?m)^".r
  def showExp(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), Console.RED   + "  -") + Console.RESET
  def showObt(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), Console.GREEN + "  +") + Console.RESET
  def showMsg(msg: Msg) = s"${msg.lineNo} ${showSev(msg.severity)}: ${msg.text.replaceAll("\n", "\\\\n")}"

  def showSev(sev: Severity) = sev match {
    case Severity.Error => "  error"
    case Severity.Warn  => "warning"
    case Severity.Info  => "   info"
  }
}

final case class TestContents(defns: List[Defn], stats: List[List[Stat]], msgs: List[List[Msg]]) {
  def ++(that: TestContents) = TestContents(
    (defns ::: that.defns).distinct,
    stats ::: that.stats,
    msgs.zipAll(that.msgs, Nil, Nil).map { case (as, bs) => (as ::: bs).distinct },
  )

  def toUnit = TestContents(defns, List(stats.flatten), msgs)
}

sealed trait TestResult
case object      TestSuccess                               extends TestResult
final case class TestFailure(msg: String)                  extends TestResult
final case class TestFailures(failures: List[TestFailure]) extends TestResult
