package shisa

import scala.language.implicitConversions

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
  import Deps._
  val SC213         = FreshCompiler2("2.13",     scalac_2_13, "")
  val SC213N        = FreshCompiler2("2.13-new", scalac_2_13, "-Xsource:3")
  val SC3M          = FreshCompiler3("3.0-migr",              "-source 3.0-migration")
  val SC3           = FreshCompiler3("3.0",                   "-source 3.0")
  val SC31M         = FreshCompiler3("3.1-migr",              "-source 3.1-migration")
  val SC31          = FreshCompiler3("3.1",                   "-source 3.1")
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
      def doTest(name: String, test: Test): TestResult = test match {
        case contents: TestContents => compile1(name, contents)
        case TestFile(name2, test)  => doTest(s"$name/$name2", test)
        case TestList(tests)        =>
          tests.zipWithIndex.map { case (test, num) =>
            doTest(s"$name.$num", test)
          }.collect {
            case tf: TestFailure   => tf
            case tfs: TestFailures => tfs.toFailure
          } match {
            case Nil      => TestSuccess(name)
            case List(tf) => tf
            case tfs      => TestFailures(name, tfs)
          }
      }
      doTest(testFile.name, testFile.test)
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

  def compile1(name: String, contents: TestContents): TestResult = {
    val compilers = mkCompilers.map(_.mkCompiler())
    val multiLine = contents.stats.sizeIs > 1
    val msgss     = if (multiLine) doLines(name, contents, compilers) else doUnit(name, contents, compilers)
    compareMsgs(name, contents, msgss)
  }

  def doUnit(name: String, contents: TestContents, compilers: List[Compiler]) = {
    doCompile1(compilers, name, toSource(contents))
  }

  def doLines(name: String, contents: TestContents, compilers: List[Compiler]) = {
    val contentss = contents.stats.map(stat => contents.copy(stats = List(stat)))
    contentss.zipWithIndex.map { case (contents, idx) =>
      doCompile1(compilers, name + idxStr(idx), toSource(contents, Some(idx)))
    }.reduce(_.zip(_).map { case (a, b) => a ::: b })
  }

  def doCompile1(compilers: List[Compiler], name: String, content: String) = {
    compilers.map(_.compile1(new SrcFile(name, content)).msgs.asScala.toList)
  }

  def toSource(contents: TestContents, pkgIdx: Option[Int] = None): String = {
    val sourceDefn = q"object Test { ..${contents.defns ::: contents.stats.flatten} }"
    val source     = pkgIdx.map(idx => Term.Name(s"p${idxStr(idx)}")) match {
      case None          => source"$sourceDefn"
      case Some(pkgName) => source"package $pkgName; $sourceDefn"
    }
    source.syntax + "\n"
  }

  def compareMsgs(name: String, contents: TestContents, obtMsgss: List[List[Msg]]): TestResult = {
    val expMsgss = contents.msgs
    val msgssZipped = expMsgss.zipAll(obtMsgss, Nil, Nil).zipAll(compilerIds, (Nil, Nil), "<unknown-compiler>")
    val testResults = for (((expMsgs, obtMsgs), compilerId) <- msgssZipped) yield {
      expMsgs.sorted.zipAll(obtMsgs.sorted, MissingExp, MissingObt).collect {
        case (exp, obt) if msgMismatch(exp, obt) => showObt(obt) + showExp(exp)
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
  def msgMismatch(exp: Msg, obt: Msg) = {
    if (exp.text == "*") exp.severity != obt.severity
    else exp != obt
  }
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

object TestContents

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

object Msgss {
  val None = ListMsgss(noMsgs)
}
sealed trait Msgss {
  def msgss: List[List[Msg]]
}

final case class ListMsgss(msgss: List[List[Msg]]) extends Msgss

//3 levels of tests:
//* template tests (top level classes/objects, Defns)
//* method tests (in classes/objects, Defns)
//* expression tests (in a method, in a class/object, Terms)

//shrinking/minimising/simplifying:
//* empty pkg -> named pkg
//* in constructor -> in method
//* object -> class
