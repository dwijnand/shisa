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
  val str = q"str"
  val m1  = q"m1"; val m2 = q"m2"
  val CR  = t"CR"; val CS = t"CS"; val CJ = t"CJ"

  val strD  = q"val str = ${Lit.String("")}"
  val m1D   = q"def $m1() = 1"
  val m2D   = q"def $m2   = 2"
  val CRD   = q"class CR"
  val CSD   = CRD.copy(name = CS).append[Stat](q"""override def $toString_   = "" """)
  val CJD   = CRD.copy(name = CJ).append[Stat](q"""override def $toString_() = "" """)
  val AnyD  = q"class Any"
  val TestD = q"object Test"

  val posShows = List(
    mkNulTest(Pos, strD,     q"$str.$toString_",   toString_, AnyD),
    mkNilTest(Pos, strD,     q"$str.$toString_()", toString_, AnyD),
    mkNulTest(Pos, CRD, q"new $CR().$toString_",   toString_, AnyD),
    mkNilTest(Pos, CRD, q"new $CR().$toString_()", toString_, AnyD),
    mkNulTest(Pos, CSD, q"new $CS().$toString_",   toString_, AnyD),
    mkNilTest(Pos, CSD, q"new $CS().$toString_()", toString_, AnyD),
    mkNulTest(Pos, CJD, q"new $CJ().$toString_",   toString_, AnyD),
    mkNilTest(Pos, CJD, q"new $CJ().$toString_()", toString_, AnyD),
  )
  val posHashP = mkNulTest(Pos, strD, q"$str.$hashHash",   hashHash, AnyD)
  val negHashM = mkNilTest(Neg, strD, q"$str.$hashHash()", hashHash, AnyD)
  val negMethP = mkNulTest(Neg,  m1D,    m1,               m1,       TestD)
  val negPropM = mkNilTest(Neg,  m2D, q"$m2()",            m2,       TestD)

  def mkNulTest(res: Res, defn: Defn, stat: Term,       meth: Term.Name, encl: Defn) = test(res, defn, stat,  autoApp(encl, meth))
  def mkNilTest(res: Res, defn: Defn, stat: Term.Apply, meth: Term.Name, encl: Defn) = test(res, defn, stat, noParams(encl, meth, Int))

  def test(res: Res, defn: Defn, stat: Stat, msgs: Msgs) = Test(defn, stat, ifMsgs(res)(msgs))
  def ifMsgs(res: Res)(msgs: Msgs) = res match { case Pos => Msgs() case Neg => msgs }

  def noParams(encl: Defn, meth: Term.Name, tp: Type.Name) = (
        Msgs.for2(_ => Msg(E, s"$tp does not take parameters"))
    ::: Msgs.for3(_ => Msg(E, s"method $meth in $encl does not take parameters"))
  )

  val posTests = posShows ::: posHashP :: Nil
  val negTests = negHashM  :: negMethP :: negPropM :: Nil
  val tests    = List(TestFile("Call.pos", posTests), TestFile("Call.neg", negTests))
}
