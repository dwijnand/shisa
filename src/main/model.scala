package shisa

import scala.language.implicitConversions

import scala.annotation.tailrec

import scala.meta._

sealed trait SV;  case object S2 extends SV;  case object S3 extends SV
sealed trait Sev; case object W  extends Sev; case object E  extends Sev

sealed trait Test {
  def ++(that: Test): TestContents = combine(this, that)

  def toUnit: TestContents = this match {
    case TestContents(defns, stats, msgs) => TestContents(defns, List(stats.flatten), msgs)
    case _                                => (this ++ noContents).toUnit
  }

  @tailrec private def combine(t1: Test, t2: Test): TestContents = (t1, t2) match {
    case (t1: TestContents, t2: TestContents) => combineContents(t1, t2)
    case (t1, TestFile(_, t2))                => combine(t1, t2)
    case (TestFile(_, t1), t2)                => combine(t1, t2)
    case (t1: TestList, t2)                   => combine(flattenList(t1), t2)
    case (t1, t2: TestList)                   => combine(t1, flattenList(t2))
  }

  private def combineContents(t1: TestContents, t2: TestContents) = TestContents(
    (t1.defns ::: t2.defns).distinct,
    t1.stats ::: t2.stats,
    t1.msgs.zipAll(t2.msgs, Nil, Nil).map { case (as, bs) => as ::: bs },
  )

  private def flattenList(t1: TestList): TestContents = t1.tests.foldLeft(noContents)(_ ++ _)

  private def noContents = TestContents(Nil, Nil, noMsgs)
}

final case class TestList(tests: List[Test])                                                     extends Test
final case class TestContents(defns: List[Defn], stats: List[List[Stat]], msgs: List[List[Msg]]) extends Test
final case class TestFile(name: String, test: Test)                                              extends Test

trait MkInMemoryTestFile {
  def name: String
  def contents: TestContents
  final def testFile: TestFile = TestFile(name, contents)
}
