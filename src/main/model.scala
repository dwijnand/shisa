package shisa

import scala.annotation.tailrec

import scala.meta._

sealed trait SV
case object S2 extends SV
case object S3 extends SV

object Test {
  val None = TestContents(Nil, Nil, noMsgs)

  def toContents(tests: List[Test]) = tests.foldLeft(Test.None)(_ ++ _)
}

sealed trait Test {
  final def ++(that: Test): TestContents = {
    @tailrec def combine(t1: Test, t2: Test): TestContents = (t1, t2) match {
      case (t1, TestFile(_, t2))                => combine(t1, t2)
      case (TestFile(_, t1), t2)                => combine(t1, t2)
      case (t1: TestList, t2)                   => combine(flatten(t1), t2)
      case (t1, t2: TestList)                   => combine(t1, flatten(t2))
      case (t1: TestContents, t2: TestContents) => combineContents(t1, t2)
    }
    def flatten(ts: TestList) = ts.tests.foldLeft(Test.None)(_ ++ _)
    combine(this, that)
  }

  private def combineContents(t1: TestContents, t2: TestContents) = {
    def defns(defns1: List[Defn], defns2: List[Defn]) = (defns1 ::: defns2).distinctBy(_.structure)
    def msgs(msgs1: List[List[Msg]], msgs2: List[List[Msg]]) = msgs1.zipAll(msgs2, Nil, Nil).map { case (as, bs) => as ::: bs }
    TestContents(defns(t1.defns, t2.defns), t1.stats ::: t2.stats, msgs(t1.msgs, t2.msgs))
  }

  @tailrec final def toContents: TestContents = this match {
    case x @ TestContents(_, _, _) => x
    case TestList(tests)           => Test.toContents(tests)
    case TestFile(_, test)         => test.toContents
  }

  final def toUnit: TestContents = {
    val TestContents(defns, stats, msgs) = toContents
    TestContents(defns, List(stats.flatten), msgs)
  }
}

final case class TestList(tests: List[Test])                                                     extends Test
final case class TestFile(name: String, test: Test)                                              extends Test
final case class TestContents(defns: List[Defn], stats: List[List[Stat]], msgs: List[List[Msg]]) extends Test

object nme {
  val getClass_ = q"getClass"
  val hashCode_ = q"hashCode"
  val hashHash  = q"##"
  val run       = q"run"
  val toString_ = q"toString"
}

object tpnme {
  val Any    = t"Any"
  val AnyRef = t"AnyRef"
  val Object = t"Object"
  val String = t"String"
}
