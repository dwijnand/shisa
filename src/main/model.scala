package shisa

import scala.language.implicitConversions

import scala.annotation.tailrec

import scala.meta._

sealed trait SV;  case object S2 extends SV;  case object S3 extends SV
sealed trait Sev; case object W  extends Sev; case object E  extends Sev

sealed trait Test {
  final def ++(that: Test): TestContents = {
    @tailrec def combine(t1: Test, t2: Test): TestContents = (t1, t2) match {
      case (t1, TestFile(_, t2))                => combine(t1, t2)
      case (TestFile(_, t1), t2)                => combine(t1, t2)
      case (t1: TestList, t2)                   => combine(flatten(t1), t2)
      case (t1, t2: TestList)                   => combine(t1, flatten(t2))
      case (t1: TestContents, t2: TestContents) => TestContents(
        (t1.defns ::: t2.defns).distinct,
        t1.stats ::: t2.stats,
        t1.msgs.zipAll(t2.msgs, Nil, Nil).map { case (as, bs) => as ::: bs },
      )
    }
    def flatten(ts: TestList) = ts.tests.foldLeft(TestContents(Nil, Nil, noMsgs))(_ ++ _)
    combine(this, that)
  }
}

final case class TestList(tests: List[Test])                                                     extends Test
final case class TestFile(name: String, test: Test)                                              extends Test
final case class TestContents(defns: List[Defn], stats: List[List[Stat]], msgs: List[List[Msg]]) extends Test
