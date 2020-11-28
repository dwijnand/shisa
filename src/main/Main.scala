package shisa

import scala.language.implicitConversions

import java.io.File
import java.net.URLClassLoader
import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.chaining._
import scala.Console.{ GREEN, RED, RESET }

import scala.meta._

import shisa.testdata._
import Severity.{ Info, Error, Warn }

object Main {
  val dotcCp = BuildInfo.scalac3Dir +: Deps.scalac_3_00
  val dotcCl = new URLClassLoader(dotcCp.map(_.toURI.toURL).toArray, getClass.getClassLoader)
  val freshCompiler3Cls  = dotcCl.loadClass("shisa.FreshCompiler3")
  val freshCompiler3Ctor = freshCompiler3Cls.getConstructor(classOf[String], classOf[Array[File]], classOf[String])

  def FreshCompiler3(id: String, cmd: String): MkCompiler =
    freshCompiler3Ctor.newInstance(id, Deps.scalac_3_00.toArray, cmd).asInstanceOf[MkCompiler]

  val SC213         = FreshCompiler2("2.13",     Deps.scalac_2_13, "")
  val SC213N        = FreshCompiler2("2.13-new", Deps.scalac_2_13, "-Xsource:3")
  val SC3M          = FreshCompiler3("3.0-migr",                   "-source 3.0-migration")
  val SC3           = FreshCompiler3("3.0",                        "-source 3.0")
  val SC31M         = FreshCompiler3("3.1-migr",                   "-source 3.1-migration")
  val SC31          = FreshCompiler3("3.1",                        "-source 3.1")
  val mkCompilers   = List(SC213, SC213N, SC3M, SC3, SC31M, SC31)
  val compilerIds   = mkCompilers.map(_.id)
  val tests         = Call.tests ::: Switch.tests ::: EtaX.tests
  val testsMap      = tests.groupMapReduce(_.name)(tf => tf)((t1, t2) => TestFile(t1.name, t1 ++ t2))
  val MissingExp    = new Msg(Error, "missing exp msg")
  val MissingObt    = new Msg(Error, "missing obt msg")

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
    val futures = testFiles.map(testFile => pool.submit[TestResult](() => {
      println(s"* ${testFile.name}")
      compile1(testFile)
    }))

    pool.shutdown()

    if (!pool.awaitTermination(10, MINUTES))
      throw new Exception("Thread pool timeout elapsed before all tests were complete!")

    val testResults  = futures.map(_.get(0, NANOSECONDS))
    val testFailures = testResults.collect {
      case tf: TestFailure   => tf
      case tfs: TestFailures => tfs.toFailure
    }

    if (testFailures.nonEmpty) {
      System.err.println(s"Test failures:")
      testFailures.foreach { case TestFailure(src, failures) => System.err.println(s"  $src $failures") }
      System.err.println(s"> run ${testFailures.map(_.name).mkString(" ")}")
      throw new Exception(s"Test failures", null, false, false) {}
    }
  }

  def compile1(testFile: TestFile): TestResult = {
    val compilers = mkCompilers.map(_.mkCompiler())
    val msgss     =
      if (testFile.contents.stats.sizeIs > 1) doLines(testFile, compilers)
      else                                     doUnit(testFile, compilers)
    compareMsgs(testFile, msgss)
  }

  def doUnit(testFile: TestFile, compilers: List[Compiler]) = {
    val contents = testFile.contents
    doCompile1(compilers, testFile.name, toSource(contents))
  }

  def doLines(testFile: TestFile, compilers: List[Compiler]) = {
    val contentss = testFile.contents.stats.map(stat => testFile.contents.copy(stats = List(stat)))
    contentss.zipWithIndex.map { case (contents, idx) =>
      doCompile1(compilers, testFile.name + idxStr(idx), toSource(contents, Some(idx)))
    }.reduce(_.zip(_).map { case (a, b) => a ::: b })
  }

  def doCompile1(compilers: List[Compiler], name: String, content: String) = {
    compilers.map(doCompile2(_, name, content))
  }

  def doCompile2(compiler: Compiler, name: String, content: String) = {
    compiler.compile1(new SrcFile(name, content)).msgs.asScala.toList
  }

  def toSource(contents: TestContents, pkgIdx: Option[Int] = None): String = {
    val sourceDefn = q"object Test { ..${contents.defns ::: contents.stats.flatten} }"
    val source     = pkgIdx.map(idx => Term.Name(s"p${idxStr(idx)}")) match {
      case None          => source"$sourceDefn"
      case Some(pkgName) => source"package $pkgName; $sourceDefn"
    }
    source.syntax + "\n"
  }

  def compareMsgs(testFile: TestFile, obtMsgss: List[List[Msg]]): TestResult = {
    val name     = testFile.name
    val expMsgss = testFile.contents.msgs
    val msgssZipped = expMsgss.zipAll(obtMsgss, Nil, Nil).zipAll(compilerIds, (Nil, Nil), "<unknown-compiler>")
    val testResults = for (((expMsgs, obtMsgs), compilerId) <- msgssZipped) yield {
      expMsgs.sorted.zipAll(obtMsgs.sorted, MissingExp, MissingObt).collect {
        case (exp, obt) if exp != obt && wildMatch(exp, obt) => showObt(obt) + showExp(exp)
      }.mkString match {
        case ""    => TestSuccess(name)
        case lines =>
          //println(s"obt:" + obtMsgs.sorted.map(showObt(_)).mkString)
          //println(s"exp:" + expMsgs.sorted.map(showExp(_)).mkString)
          TestFailure(name, s"$name: message mismatch ($compilerId) ($RED-obtained$RESET/$GREEN+expected$RESET):$lines")
      }
    }
    testResults.collect { case tf: TestFailure => tf } match {
      case Nil          => TestSuccess(name)
      case testFailures => TestFailures(name, testFailures)
    }
  }

  implicit def orderingMsg: Ordering[Msg]      = Ordering.by((msg: Msg) => (msg.severity, msg.text))
  implicit def orderingSev: Ordering[Severity] = Ordering.by { case Error => 1 case Warn => 2 case Info => 3 }

  val LineStart              = "(?m)^".r
  def showObt(msg: Msg)      = "\n" + LineStart.replaceAllIn(showMsg(msg), RED   + "  -") + RESET
  def showExp(msg: Msg)      = "\n" + LineStart.replaceAllIn(showMsg(msg), GREEN + "  +") + RESET
  def showMsg(msg: Msg)      = s"${showSev(msg.severity)}: ${msg.text.replaceAll("\n", "\\\\n")}"
  def showSev(sev: Severity) = sev match { case Error => "  error" case Warn => "warning" case Info => "   info" }
  def wildMatch(exp: Msg, obt: Msg) = exp.text == "*" && exp.severity != obt.severity
}

object Test {
  val None = TestContents(Nil, Nil, noMsgs)
}

sealed trait Test {
  /* @tailrec */ def ++(that: Test): TestContents = (this, that) match {
    case (t1: TestContents, t2: TestContents) => t1 ++ t2
    case (t1, TestFile(_, t2))                => t1 ++ t2
    case (TestFile(_, t1), t2)                => t1 ++ t2
    case (t1: TestList, t2)                   => t1.flatten ++ t2
    case (t1, t2: TestList)                   => t1 ++ t2.flatten
  }

  def contents: TestContents = this ++ Test.None
}

final case class TestList(tests: List[Test]) extends Test {
  def flatten = tests.foldLeft(Test.None)(_ ++ _)
}

final case class TestContents(defns: List[Defn], stats: List[List[Stat]], msgs: List[List[Msg]]) extends Test {
  def ++(that: TestContents) = TestContents(
    (defns ::: that.defns).distinct,
    stats ::: that.stats,
    msgs.zipAll(that.msgs, Nil, Nil).map { case (as, bs) => as ::: bs },
  )

  def toUnit = TestContents(defns, List(stats.flatten), msgs)
}

final case class TestFile(name: String, test: Test) extends Test

sealed trait TestResult { def name: String }
final case class TestSuccess(name: String)                               extends TestResult
final case class TestFailure(name: String, msg: String)                  extends TestResult
final case class TestFailures(name: String, failures: List[TestFailure]) extends TestResult {
  def toFailure: TestFailure = TestFailure(name, failures.map(tf => s"\n  ${tf.msg}").mkString)
}
