package shisa

import scala.language.implicitConversions

import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.chaining._
import scala.Console.{ GREEN, RED, RESET }

import scala.meta._
import MsgsImport._

//3 levels of tests:
//* template tests (top level classes/objects, Defns)
//* method tests (in classes/objects, Defns)
//* expression tests (in a method, in a class/object, Terms)
//shrinking/minimising/simplifying:
//* empty pkg -> named pkg
//* in constructor -> in method
//* object -> class
//* trait  -> abstract class
//* implemented method -> abstract method
//--
//* saying TypeError1 and RefCheckError1 can go together: wrong immediately
//* saying TypeError1 and TypeError2     must be separate: wrong eventually when randomly tested together and finding both
object Main {
  import Deps._
  val SC2         = mkScalac2("2.13")
  val SC2N        = mkScalac2("2.13-new", "-Xsource:3")
  val SC3M        = mkScalac3("3.0-migr", "-source 3.0-migration")
  val SC3         = mkScalac3("3.0",      "-source 3.0")
  val SC31M       = mkScalac3("3.f-migr", "-source future-migration")
  val SC31        = mkScalac3("3.f",      "-source future")
  val mkCompilers = List(SC2, SC2N, SC3M, SC3, SC31M, SC31)
  val tests       = (Call.tests ::: Switch.tests ::: EtaX.tests).sortBy(_.name)
  val testsMap    = tests.groupMapReduce(_.name)(tf => tf)((t1, t2) => TestFile(t1.name, Test.combine(t1, t2)))

  def main(args: Array[String]): Unit = args.toList match {
    case Nil  =>
      println(s"Files: ${tests.map(_.name).mkString("[", ", ", "]")}")
      runTests(tests)
    case args => args.partitionMap(name => testsMap.get(name).toRight(name)) match {
      case (Nil, tests) => runTests(tests)
      case (missing, _) => sys.error(s"Missing test files: ${missing.mkString("[", ", ", "]")}")
    }
  }

  def runTests(tests: List[TestFile]): Unit = {
    val pool    = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val futures = tests.sortBy(_.name).distinctBy(_.name).map(test => pool.submit[List[TestFailure]](() => {
      //println(s"* ${test.name}")
      prepAndRunTest(test)
    }))
    pool.shutdown()

    if (!pool.awaitTermination(10, MINUTES))
      throw new Exception("Thread pool timeout elapsed before all tests were complete!")

    val testFailures = futures.flatMap(_.get(0, NANOSECONDS))
    if (testFailures.nonEmpty) {
      System.err.println("Test failures:")
      testFailures.foreach { case TestFailure(name, msg) => System.err.println(s"  $name: $msg") }
      System.err.println(s"${testFailures.size} failures.")
      System.err.println(s"> run ${testFailures.map(_.name).mkString(" ")}")
      throw new Exception("Test failures", null, false, false) {}
    }
  }

  def prepAndRunTest(test: TestFile): List[TestFailure] = {
    runTest(mkCompilers.map(_.mkCompiler), test.name, test.test)
  }

  def runTest(compilers: List[Compiler], name: String, test: Test): List[TestFailure] = test match {
    case test: TestContents    => runTest1(compilers, name, test)
    case TestFile(name2, test) => runTest(compilers, s"$name/$name2", test)
    case TestList(tests)       => tests.zipWithIndex.flatMap { case (test, num) => runTest(compilers, s"$name.$num", test) }
  }

  def runTest1(compilers: List[Compiler], name: String, test: TestContents): List[TestFailure] = {
    val srcSyntax = toObject(test).syntax + "\n"
    val srcFiles  = if (test.stats.sizeIs > 1) {
      test.stats.zipWithIndex.map { case (stats, idx) =>
        val obj    = toObject(test.copy(stats = List(stats)))
        val suffix = if (idx < 0) "" else if (idx < 10) s"0$idx" else s"$idx"
        val stat   = if (suffix == "") obj else Pkg(Term.Name(s"p$suffix"), List(obj))
        SrcFile(name + suffix, stat.syntax + "\n")
      }
    } else List(SrcFile(name, srcSyntax))
    val msgsss = srcFiles.map(srcFile => compilers.map(_.compile1(srcFile)))
    val msgs   = msgsss.reduce(_.zip(_).map { case (a, b) => a ::: b })
    compareMsgs(name, test.msgs, msgs, srcSyntax)
  }

  def toObject(test: TestContents): Defn.Object = q"object Test { ..${test.defns ::: test.stats} }"

  def compareMsgs(name: String, expMsgs: Msgs, obtMsgs: List[List[Msg]], srcSyntax: String): List[TestFailure] = {
    val msgsZipped = expMsgs.toList.zipAll(obtMsgs, Nil, Nil).zipAll(mkCompilers.map(_.id), (Nil, Nil), "<unknown-compiler>")
    for {
      ((expMsgs, obtMsgs), compilerId) <- msgsZipped
      lines = {
        expMsgs.sorted.zipAll(obtMsgs.sorted, null, null).collect {
          case (exp, obt) if exp != obt => showObt(obt) + showExp(exp)
        }.mkString
      } if lines.nonEmpty
    } yield {
      //println(s"obt:" + obtMsgs.sorted.map(showObt(_)).mkString)
      //println(s"exp:" + expMsgs.sorted.map(showExp(_)).mkString)
      TestFailure(name, s"message mismatch ($compilerId) ($RED- obtained$RESET/$GREEN+ expected$RESET):$lines") // \nsource:\n$srcSyntax")
    }
  }

  final case class TestFailure(name: String, msg: String)

  val LineStart         = "(?m)^".r
  def showObt(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), RED   + "  -") + RESET
  def showExp(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), GREEN + "  +") + RESET
  def showMsg(msg: Msg) = msg match {
    case null         => s"   ??? : <missing msg>"
    case Msg(W, text) => s"warning: ${showText(text)}"
    case Msg(E, text) => s"  error: ${showText(text)}"
  }
  def showText(text: String) = text.replaceAll("\n", "\\\\n")

  implicit def orderingMsg: Ordering[Msg] = Ordering.by((msg: Msg) => (msg.sev, msg.text))
  implicit def orderingSev: Ordering[Sev] = Ordering.by { case E => 1 case W => 2 }
}
