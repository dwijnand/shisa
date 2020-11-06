package shisa

import scala.language.implicitConversions

import java.io.File
import java.net.URLClassLoader
import java.nio.file._
import java.util.concurrent.Executors

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.chaining._
import scala.Console.{ GREEN, RED, RESET }

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
  val testsMap      = tests.groupMapReduce(_.name)(tf => tf)((tf1, tf2) => TestFile(tf1.name, tf1.contents ++ tf2.contents))
  val MissingExp    = new Msg(Severity.Error, 0, "missing exp msg")
  val MissingObt    = new Msg(Severity.Error, 0, "missing obt msg")

  def idxStr(idx: Int) = if (idx < 10) s"0$idx" else s"$idx"

  def main(args: Array[String]): Unit = {
    val testFiles = args.toList match {
      case Nil  => tests.sortBy(_.name).tap(tests => println(s"Files: ${tests.map(_.name).mkString("[", ", ", "]")}"))
      case args =>
        val (missing, tests) = args.partitionMap(name => testsMap.get(name).toRight(Left(name)))
        if (missing.isEmpty) tests
        else sys.error(s"Missing test files: ${missing.mkString("[", ", ", "]")}")
    }

    val pool    = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val futures = testFiles.map(tf => pool.submit[TestResult](() => compile1(tf, mkCompilers)))

    pool.shutdown()

    if (!pool.awaitTermination(10, MINUTES))
      throw new Exception("Thread pool timeout elapsed before all tests were complete!")

    val testResults  = futures.map(_.get(0, NANOSECONDS))
    val testFailures = testResults.collect {
      case TestFailure(src, msg)       => s"$src: $msg"
      case TestFailures(src, failures) => s"$src:\n${failures.map(tf => s"  ${tf.msg}").mkString("\n")}"
    }

    if (testFailures.nonEmpty) {
      System.err.println(s"Test failures:\n${testFailures.mkString("\n")}")
      throw new Exception(s"Test failures", null, false, false) {}
    }
  }

  def compile1(testFile: TestFile, mkCompilers: List[MkCompiler]): TestResult = {
    println(s"* ${testFile.name}")
    val compilers = mkCompilers.map(_.mkCompiler())
    if (testFile.contents.stats.sizeIs > 1) doLines(testFile, compilers)
    else                                     doUnit(testFile, compilers)
  }

  def doUnit(testFile: TestFile, compilers: List[Compiler]): TestResult = {
    compareMsgs(testFile, writeAndCompile(compilers, toSource(testFile.contents)))
  }

  def doLines(testFile: TestFile, compilers: List[Compiler]) = {
    val msgss = testFile.contents.stats
      .map(stat => testFile.contents.copy(stats = List(stat)))
      .zipWithIndex.map { case (contents, idx) =>
        writeAndCompile(compilers, toSource(contents, Some(idx)))
      }.reduce(_.zip(_).map { case (a, b) => a ::: b })
    compareMsgs(testFile, msgss)
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
    Files.createDirectories(src.getParent)
    Files.writeString(src, sourceStr)
    compilers.map(_.compile1(src)).map(_.msgs.asScala.toList)
  }

  def compareMsgs(testFile: TestFile, obtMsgss: List[List[Msg]]): TestResult = {
    val TestFile(name, TestContents(_, _, expMsgss)) = testFile
    val msgssZipped = expMsgss.zipAll(obtMsgss, Nil, Nil).zipAll(compilerIds, (Nil, Nil), "<unknown-compiler>")
    val testResults = for (((expMsgs, obtMsgs), compilerId) <- msgssZipped) yield {
      expMsgs.zipAll(obtMsgs, MissingExp, MissingObt).collect {
        case (exp, obt) if exp != obt => showExp(exp) + showObt(obt)
      }.mkString match {
        case ""    => TestSuccess(name)
        case lines => TestFailure(name, s"$name: message mismatch ($compilerId) ($RED-expected$RESET/$GREEN+obtained$RESET):$lines")
      }
    }
    testResults.collect { case tf: TestFailure => tf } match {
      case Nil          => TestSuccess(name)
      case testFailures => TestFailures(name, testFailures)
    }
  }

  val LineStart         = "(?m)^".r
  def showExp(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), RED   + "  -") + RESET
  def showObt(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), GREEN + "  +") + RESET
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
    msgs.zipAll(that.msgs, Nil, Nil).map { case (as, bs) => as ::: bs },
  )

  def toUnit = TestContents(defns, List(stats.flatten), msgs)
}

final case class TestFile(name: String, contents: TestContents)

sealed trait TestResult { def name: String }
final case class TestSuccess(name: String)                               extends TestResult
final case class TestFailure(name: String, msg: String)                  extends TestResult
final case class TestFailures(name: String, failures: List[TestFailure]) extends TestResult
