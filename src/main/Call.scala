package shisa

import scala.meta._, contrib._, Transformations._
import nme._, tpnme._

// things that matter:
// * call `##` as a nilary method (`x.##()`)
// * switch calling
// things that don't matter:
// * defined as nullary vs nilary
// * receiver type (Any, AnyRef, Object, String)
// * with receiver vs on implied `this` receiver
// * value class
// * case class
// * method name (hashCode, toString, getClass)
// * method result tpe (hashCode, toString, getClass)
// todo matter?
// * defined as enriched method
// todo don't matter:
// * defn + call in separate vs joint compilation
// * java-defined
// * the enclosing: method, nesting, constructors
// * other compiler settings
object Call {
  def tests: List[TestFile] = negTests :: posTests :: Nil

  def negTests = TestFile("Call.neg", List(
    List(                    mkNegNulTest(q"def m1() = 1", q"m1",          q"object Test")),
    List(                    mkNegNilTest(q"def m2   = 2", q"m2()", q"m2", q"object Test")),
    for (defn <- vals) yield mkNegNilTest(defn, q"${defn.inst}.$hashHash()", hashHash, q"class Any"),
  ).flatten)
  def posTests = TestFile("Call.pos", List(
    for (               defn <- vals) yield mkPosNulTest(defn, defn.inst, hashHash),
    for (meth <- meths; defn <- vals; test <- mkPosTests(defn, defn.inst, meth     )) yield test,
    for (defn <- clss;                test <- mkPosTests(defn, defn.inst, toString_)) yield test,
    for (defn <- clsR;                test <- mkPosTests(defn, defn.inst, run      )) yield test,
  ).flatten)

  def mkPosTests  (defn: Defn, inst: Term,       meth: Term.Name            ) = List(mkPosNulTest(defn, inst, meth), mkPosNilTest(defn, inst, meth))
  def mkPosNulTest(defn: Defn, inst: Term,       meth: Term.Name            ) = Test(defn, nul(inst, meth), Msgs())
  def mkPosNilTest(defn: Defn, inst: Term,       meth: Term.Name            ) = Test(defn, nil(inst, meth), Msgs())
  def mkNegNulTest(defn: Defn,                   meth: Term.Name, encl: Defn) = Test(defn,            meth, autoApp(q"object Test", meth))
  def mkNegNilTest(defn: Defn, stat: Term.Apply, meth: Term.Name, encl: Defn) = Test(defn,            stat, noParams(encl, meth, Int))

  def   CR = q"class   CR".withRunnable
  def  CCR = q"class  CCR".withRunnable.toCaseClass
  def  VCR = q"class  VCR".toValueClass(param"val x: String")
  def VCCR = q"class VCCR".toValueClass(param"val x: String").toCaseClass

  def clsR = List(CR, CCR)
  def clss = (clsR ::: List(VCR, VCCR)).flatMap { cls =>
    val s = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "S")).append[Stat](q"override def $toString_   = ${Lit.String("")}")
    val j = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "J")).append[Stat](q"override def $toString_() = ${Lit.String("")}")
    List(cls, s, j)
  }
  def mkV(name: Term.Name, tp: Type.Name) = q"val ${name.asPat}: $tp = ${Lit.String("")}"
  def vals  = List(mkV(q"any", Any), mkV(q"ref", AnyRef), mkV(q"obj", Object), mkV(q"str", String))
  def meths = List(toString_, getClass_, hashCode_)

  def nul(qual: Term, name: Term.Name): Term        = qual match { case Lit.Null() => q"$name"   case _ => q"$qual.$name"   }
  def nil(qual: Term, name: Term.Name): Term.Apply  = qual match { case Lit.Null() => q"$name()" case _ => q"$qual.$name()" }

  def noParams(encl: Defn, meth: Term.Name, tp: Type.Name) = (
        Msgs.for2(_ => Msg(E, s"$tp does not take parameters"))
    ::: Msgs.for3(_ => Msg(E, s"method $meth in $encl does not take parameters"))
  )

  implicit class myDefnValOps(private val valDefn: Defn.Val) extends AnyVal {
    def inst = valDefn.pats match {
      case Pat.Var(name) :: Nil => name
      case pats                 => throw sys.error(s"Unexpected Defn.Val pats: ${pats.map(_.syntax)} ${pats.map(_.structure)}")
    }
  }

  implicit class myDefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def withRunnable = cls.addInit(init"Runnable").withStats(List(q"def run() = ()"))

    def inst: Term = {
      val args = if (cls.isValueClass) List(Lit.String("")) else Nil
      if (cls.isCaseClass) q"${cls.name.asTerm}(..$args)" else q"new ${cls.name}(..$args)"
    }
  }
}
