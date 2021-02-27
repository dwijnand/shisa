package shisa

import scala.annotation.tailrec

import scala.meta._, contrib._

object Test {
  def apply(defn: Defn, stat: Stat, msgs: Msgs) = TestContents(List(defn), List(stat), msgs)

  val NoTest = TestContents(Nil, Nil, Msgs())

  def toContents(tests: List[Test]): TestContents  = tests.foldLeft(NoTest)(combineTest)

  @tailrec def combineTest(t1: Test, t2: Test): TestContents = (t1, t2) match {
    case (t1, TestFile(_, t2))                => combineTest(t1, t2)
    case (TestFile(_, t1), t2)                => combineTest(t1, t2)
    case (t1: TestList, t2)                   => combineTest(toContents(t1.tests), t2)
    case (t1, t2: TestList)                   => combineTest(t1, toContents(t2.tests))
    case (t1: TestContents, t2: TestContents) => combineContents(t1, t2)
  }

  private def combineDefns(t1: TestContents, t2: TestContents) = (t1.defns ::: t2.defns).distinctBy(_.structure)
  private def combineMsgss(t1: TestContents, t2: TestContents) = t1.msgs ::: t2.msgs

  private def combineContents(t1: TestContents, t2: TestContents) = TestContents(combineDefns(t1, t2), t1.stats ::: t2.stats, combineMsgss(t1, t2))
}

sealed trait Test {
  final def ++(that: Test): TestContents = Test.combineTest(this, that)

  @tailrec final def toContents: TestContents = this match {
    case x @ TestContents(_, _, _) => x
    case TestList(tests)           => Test.toContents(tests)
    case TestFile(_, test)         => test.toContents
  }
}

final case class TestList(tests: List[Test])                                    extends Test
final case class TestFile(name: String, test: Test)                             extends Test
final case class TestContents(defns: List[Defn], stats: List[Stat], msgs: Msgs) extends Test

object TestFile {
  def apply(name: String, ts: List[TestContents]): TestFile = TestFile(name, Test.toContents(ts))
}
