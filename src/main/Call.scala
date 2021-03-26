package shisa

import scala.meta._, contrib._, Transformations._
import nme._, tpnme._

// things that matter:
// * call `##` as a nilary method (`x.##()`)
// * switch calling
// things that don't matter:
// * defined as nullary vs nilary (when called as such)
// things that don't matter (removed):
// * receiver type (Any, AnyRef, Object, String)
// * with receiver vs on implied `this` receiver
// * value class, case class
// * method name (hashCode, getClass, run)
// * method result tpe (int, Class[?], void)
// * java-defined (run)
// todo matter?
// * defined as enriched method
// todo don't matter:
// * defn + call in separate vs joint compilation
// * the enclosing: method, nesting, constructors
// * other compiler settings
object Call {
  def tests: List[TestFile] = TestFile("Call.neg", negTests) :: TestFile("Call.pos", posTests) :: Nil

  def negTests = List(
    mkNegNulTest(q"def m1() = 1", q"m1",          q"object Test"),
    mkNegNilTest(q"def m2   = 2", q"m2()", q"m2", q"object Test"),
    mkNegNilTest(val1, q"${val1.inst}.$hashHash()", hashHash, q"class Any"),
  )
  def posTests = List(
    List(                   mkPosNulTest (val1, val1.inst, hashHash)),
                               mkPosTests(val1, val1.inst, toString_),
    for (defn <- clss; test <- mkPosTests(defn, defn.inst, toString_)) yield test
  ).flatten

  def mkNegNulTest(defn: Defn,                   meth: Term.Name, encl: Defn) = Test(defn,            meth, autoApp(q"object Test", meth))
  def mkNegNilTest(defn: Defn, stat: Term.Apply, meth: Term.Name, encl: Defn) = Test(defn,            stat, noParams(encl, meth, Int))
  def mkPosNulTest(defn: Defn, inst: Term,       meth: Term.Name            ) = Test(defn, nul(inst, meth), Msgs())
  def mkPosNilTest(defn: Defn, inst: Term,       meth: Term.Name            ) = Test(defn, nil(inst, meth), Msgs())
  def mkPosTests  (defn: Defn, inst: Term,       meth: Term.Name            ) = List(mkPosNulTest(defn, inst, meth), mkPosNilTest(defn, inst, meth))

  def val1 = q"val str = ${Lit.String("")}"
  def clss = {
    val c = q"class CR"
    val s = c.copy(name = Type.Name(c.name.value.stripSuffix("R") + "S")).append[Stat](q"override def $toString_   = ${Lit.String("")}")
    val j = c.copy(name = Type.Name(c.name.value.stripSuffix("R") + "J")).append[Stat](q"override def $toString_() = ${Lit.String("")}")
    List(c, s, j)
  }

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
    def inst: Term = q"new ${cls.name}()"
  }
}
