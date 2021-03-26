package shisa

import scala.annotation.tailrec

import scala.meta._, contrib._

object Test {
  def apply(defn: Defn, stat: Stat, msgs: Msgs)    = TestContents(List(defn), List(stat), msgs)
  val NoTest: TestContents                         = TestContents(Nil, Nil, Msgs())
  def toContents(tests: List[Test]): TestContents  = tests.foldLeft(NoTest)(combine)

  @tailrec def combine(t1: Test, t2: Test): TestContents = (t1, t2) match {
    case (t1, TestFile(_, t2))                => combine(t1, t2)
    case (TestFile(_, t1), t2)                => combine(t1, t2)
    case (t1: TestList, t2)                   => combine(toContents(t1.tests), t2)
    case (t1, t2: TestList)                   => combine(t1, toContents(t2.tests))
    case (t1: TestContents, t2: TestContents) => TestContents(combineDefns(t1, t2), t1.stats ::: t2.stats, t1.msgs ::: t2.msgs)
  }

  private def combineDefns(t1: TestContents, t2: TestContents) = (t1.defns ::: t2.defns).distinctBy(_.structure)
}

sealed trait Test
final case class TestList(tests: List[Test])                                    extends Test
final case class TestFile(name: String, test: Test)                             extends Test
final case class TestContents(defns: List[Defn], stats: List[Stat], msgs: Msgs) extends Test

object TestFile {
  def apply(name: String, ts: List[TestContents]): TestFile = TestFile(name, Test.toContents(ts))
}
