package shisa

import scala.meta._, contrib._

// * receiver type: scala-defined, java-defined/String, * value class, * case class + Any, AnyRef, Object
// * method defn: nullary/nilary, * java-defined + hashCode, ##, toString, getClass
// * invoked nullary/nilary
object Call {
  def tests: List[TestFile] =
    toNegAndPos("Call", allTests) ::: methP(q"foo") :: propM(q"bar") :: Nil

  val cls1 = List(q"class  CR".withRunnable, q"class  CCR".withRunnable.toCaseClass)
  val cls2 = List(q"class VCR".toValueClass, q"class VCCR".toValueClass.toValueClass)
  val clss = cls1 ::: cls2
  val clsV = clss.flatMap { cls =>
    val s = cls.copy(name = Type.Name(cls.name.value + "S")).addStat(q"override def ${nme.toString_}   = $ns")
    val j = cls.copy(name = Type.Name(cls.name.value + "J")).addStat(q"override def ${nme.toString_}() = $ns")
    List(cls, s, j)
  }
  import tpnme._
  def mkV(name: Term.Name, tp: Type.Name) = q"val ${name.asPat}: $tp = ${Lit.String("")}"
  val vals = List(mkV(q"any", Any), mkV(q"ref", AnyRef), mkV(q"obj", Object), mkV(q"str", String))

  def nul(qual: Term, name: Term.Name): Term       = q"$qual.$name"
  def nil(qual: Term, name: Term.Name): Term       = q"$qual.$name()"
  def two(qual: Term, name: Term.Name): List[Term] = List(nul(qual, name), nil(qual, name))

  def mkErr2(tp: String)                = err(                 s"$tp does not take parameters")
  def mkErr3(meth: String, enc: String) = err(s"method $meth in $enc does not take parameters")
  def mkErrs(enc: String, meth: String, tp: String) = multi(mkErr2(tp), mkErr3(meth, enc))

  val allTests = List(
    for (x <- vals; stat  = nil(x.inst, nme.hashHash )) yield mkTest(x, stat, mkErrs("class Any", "##", "Int")),
    for (x <- vals; stat  = nul(x.inst, nme.hashHash )) yield mkTest(x, stat, noMsgs),
    for (x <- vals; stat <- two(x.inst, nme.toString_)) yield mkTest(x, stat, noMsgs),
    for (x <- vals; stat <- two(x.inst, nme.getClass_)) yield mkTest(x, stat, noMsgs),
    for (x <- vals; stat <- two(x.inst, nme.hashCode_)) yield mkTest(x, stat, noMsgs),
    for (x <- clss; stat <- two(x.inst, nme.toString_)) yield mkTest(x, stat, noMsgs),
    for (x <- cls1; stat <- two(x.inst, nme.run      )) yield mkTest(x, stat, noMsgs),
  ).flatten

  def methPMsgs(meth: Term.Name) = multi3 {
    case (S2,   _) => List(Msg(W,   autoApp2(meth.value)))
    case (S3, sev) => List(Msg(sev, autoApp3(meth.value)))
  }
  def propMMsgs(meth: Term.Name) = mkErrs("object Test", meth.value, "Int")

  def methP_T(meth: Term.Name) = mkTest(q"def $meth() = 1", q"$meth",   methPMsgs(meth))
  def propM_T(meth: Term.Name) = mkTest(q"def $meth   = 2", q"$meth()", propMMsgs(meth))
  def methP(meth: Term.Name)   = TestFile("Call.meth_p", methP_T(meth))
  def propM(meth: Term.Name)   = TestFile("Call.prop_m", propM_T(meth))

}
