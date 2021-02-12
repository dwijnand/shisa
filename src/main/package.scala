package shisa

import scala.annotation.tailrec

import scala.meta._, classifiers.{ Classifiable, Classifier }, contrib._

sealed trait Test {
  final def ++(that: Test): TestContents = combineTest(this, that)

  @tailrec final def toContents: TestContents = this match {
    case x @ TestContents(_, _, _) => x
    case TestList(tests)           => shisa.toContents(tests)
    case TestFile(_, test)         => test.toContents
  }
}

final case class TestList(tests: List[Test])                                               extends Test
final case class TestFile(name: String, test: Test)                                        extends Test
final case class TestContents(defns: List[Defn], stats: List[Stat], msgs: List[List[Msg]]) extends Test

object `package` {
  val noMsgs            = List(Nil, Nil, Nil, Nil, Nil, Nil)
  def  err(str: String) = Msg(E, str)
  val NoTest            = TestContents(Nil, Nil, noMsgs)

  @tailrec def combineTest(t1: Test, t2: Test): TestContents = (t1, t2) match {
    case (t1, TestFile(_, t2))                => combineTest(t1, t2)
    case (TestFile(_, t1), t2)                => combineTest(t1, t2)
    case (t1: TestList, t2)                   => combineTest(toContents(t1.tests), t2)
    case (t1, t2: TestList)                   => combineTest(t1, toContents(t2.tests))
    case (t1: TestContents, t2: TestContents) => combineContents(t1, t2)
  }

  private def combineDefns(t1: TestContents, t2: TestContents) = (t1.defns ::: t2.defns).distinctBy(_.structure)
  private def combineMsgss(t1: TestContents, t2: TestContents) = t1.msgs.zipAll(t2.msgs, Nil, Nil).map { case (as, bs) => as ::: bs }

  private def combineContents(t1: TestContents, t2: TestContents) = TestContents(combineDefns(t1, t2), t1.stats ::: t2.stats, combineMsgss(t1, t2))

  def toContents(tests: List[Test]): TestContents           = tests.foldLeft(NoTest)(combineTest)
  def mkTest(defn: Defn, stat: Stat, msgs: List[List[Msg]]) = TestContents(List(defn), List(stat), msgs)
  def mkFile(name: String, ts: List[TestContents])          = TestFile(name, toContents(ts))

  def msgs2or3(m2: Sev => Msg, m3: Sev => Msg) = List(List(m2(W)), List(m2(E)), List(m3(W)), List(m3(E)), List(m3(W)), List(m3(E)))

  def msgsFor2R(m:        Msg) = List(List(m), Nil, Nil, Nil, Nil, Nil)
  def msgsFor3 (m:        Msg) = List(Nil, Nil, List(m), List(m), List(m), List(m))
  def msgsFor31(f: Sev => Msg) = List(Nil, Nil, Nil, Nil, List(f(W)), List(f(E)))

  def autoApp(meth: Term.Name) = List(
    List(Msg(W, autoApp2(meth.value))),
    List(Msg(W, autoApp2(meth.value))),
    List(Msg(W, autoApp3(meth.value))),
    List(Msg(E, autoApp3(meth.value))),
    List(Msg(E, autoApp3(meth.value))),
    List(Msg(E, autoApp3(meth.value))),
  )

  def autoApp2(meth: String) =
    s"""Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method $meth,
       |or remove the empty argument list from its definition (Java-defined methods are exempt).
       |In Scala 3, an unapplied method like this will be eta-expanded into a function.""".stripMargin
  def autoApp3(meth: String) = s"method $meth must be called with () argument"

  implicit class ListOps[T](private val xs: List[T]) extends AnyVal {
    def has[U](implicit classifier: Classifier[T, U]): Boolean = xs.exists( classifier(_))

    def  appendOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else xs :+ x
    def prependOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else x :: xs
  }

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = param.copy(mods = param.mods.appendOnce(Mod.ValParam()))
  }

  implicit class ExtensionAdders[A](a: A) {
    def addStat(b: Stat)(implicit E: Extract[A, Stat], R: Replace[A, Stat]): A =
      R.replace(a, E.extract(a) :+ b)
  }

  implicit class DefnValOps(private val valDefn: Defn.Val) extends AnyVal {
    def inst = valDefn.pats match {
      case Pat.Var(name) :: Nil => name
      case pats                 => throw sys.error(s"Unexpected Defn.Val pats: ${pats.map(_.syntax)} ${pats.map(_.structure)}")
    }
  }

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = cls.templ.inits.prependOnce(init)))

    def toCaseClass = cls.copy(
      mods = cls.mods.appendOnce(Mod.Case()),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil     => List(Nil)
        case paramss => paramss.map(_.map(_.notValParam))
      }),
    )

    def toValueClass = cls.addInit(init"AnyVal").copy(
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil           => List(List(param"val x: String"))
        case List(List(p)) => List(List(p.toValParam))
        case paramss       => sys.error(s"Can't toValueClass ${cls.name} b/c of paramss: $paramss")
      }),
    )

    def withRunnable = addInit(init"Runnable").withStats(List(q"def run() = ()"))

    def isCaseClass  = cls.hasMod(Mod.Case())
    def isValueClass = cls.templ.inits.exists { case Init(Type.Name("AnyVal"), Name(""), Nil) => true case _ => false }

    def inst: Term = {
      val args = if (isValueClass) List(Lit.String("")) else Nil
      if (isCaseClass) q"${cls.name.asTerm}(..$args)" else q"new ${cls.name}(..$args)"
    }
  }
}

sealed trait SV;         case object S2   extends SV;         case object S3   extends SV
sealed trait MethOrProp; case object Meth extends MethOrProp; case object Prop extends MethOrProp

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
  val Int    = t"Int"
  val Object = t"Object"
  val String = t"String"
}
