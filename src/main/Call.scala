package shisa

import scala.meta._, contrib._

// * receiver type: scala-defined, java-defined/String, * value class, * case class + Any, AnyRef, Object
// * method defn: nullary/nilary, * java-defined + hashCode, ##, toString, getClass
// * invoked nullary/nilary
// in order:
// * define method as meth, call as prop
// * define method as prop, call as method
// * Any/AnyRef/Object/String value, call ##() -> neg
// * Any/AnyRef/Object/String value, call ##   -> pos
// * Any/AnyRef/Object/String value, call toString/() + getClass + hashCode
// * class/case class/value class/value case class, call toString/()
// * class/case class, call run/()
// things that matter:
// * `##` vs rest
// things that don't matter:
// * scala/java-defined
// * receiver type (Any, AnyRef, Object, String)
// * receiver vs no receiver (this receiver)
// * value class
// * case class
// * method name/result tpe (hashCode, toString, getClass)
// todo:
// * separate/joint compilation
// * java-defined
// * generalise enclosing: method, nesting, constructors
// * other settings
object Call {
  def tests: List[TestFile] = methP(q"foo") :: propM(q"bar") :: negValTests :: posValTests :: clsTests :: Nil

  def methP(meth: Term.Name) = TestFile("Call.meth_p", mkTest(q"def $meth() = 1", q"$meth",   methPMsgs(meth)))
  def propM(meth: Term.Name) = TestFile("Call.prop_m", mkTest(q"def $meth   = 2", q"$meth()", propMMsgs(meth)))

  val cls1 = List(q"class  CR".withRunnable, q"class  CCR".withRunnable.toCaseClass)
  val cls2 = List(q"class VCR".toValueClass, q"class VCCR".toValueClass.toCaseClass)
  val clsV = (cls1 ::: cls2).flatMap { cls =>
    val s = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "S")).addStat(q"override def ${nme.toString_}   = $ns")
    val j = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "J")).addStat(q"override def ${nme.toString_}() = $ns")
    List(cls, s, j)
  }
  import tpnme._
  def mkV(name: Term.Name, tp: Type.Name) = q"val ${name.asPat}: $tp = ${Lit.String("")}"
  val vals = List(mkV(q"any", Any), mkV(q"ref", AnyRef), mkV(q"obj", Object), mkV(q"str", String))

  def nul(qual: Term, name: Term.Name): Term       = q"$qual.$name"
  def nil(qual: Term, name: Term.Name): Term       = q"$qual.$name()"
  def two(qual: Term, name: Term.Name): List[Term] = List(nul(qual, name), nil(qual, name))

  val negValTests = mkFile("Call.negVal",
    for (x <- vals; stat  = nil(x.inst, nme.hashHash )) yield mkTest(x, stat, mkErrs("class Any", "##", "Int")),
  )

  val posValTests = mkFile("Call.posVal", List(
    for (x <- vals; stat  = nul(x.inst, nme.hashHash )) yield mkTest(x, stat, noMsgs),
    for (x <- vals; stat <- two(x.inst, nme.toString_)) yield mkTest(x, stat, noMsgs),
    for (x <- vals; stat <- two(x.inst, nme.getClass_)) yield mkTest(x, stat, noMsgs),
    for (x <- vals; stat <- two(x.inst, nme.hashCode_)) yield mkTest(x, stat, noMsgs),
  ).flatten)

  val clsTests = mkFile("Call.cls", List(
    for (x <- clsV; stat <- two(x.inst, nme.toString_)) yield mkTest(x, stat, noMsgs),
    for (x <- cls1; stat <- two(x.inst, nme.run      )) yield mkTest(x, stat, noMsgs),
  ).flatten)

  def methPMsgs(meth: Term.Name) = multi3 {
    case (S2,   _) => List(Msg(W,   autoApp2(meth.value)))
    case (S3, sev) => List(Msg(sev, autoApp3(meth.value)))
  }
  def propMMsgs(meth: Term.Name) = mkErrs("object Test", meth.value, "Int")

  def mkErr2(tp: String)                = err(                 s"$tp does not take parameters")
  def mkErr3(enc: String, meth: String) = err(s"method $meth in $enc does not take parameters")
  def mkErrs(enc: String, meth: String, tp: String) = multi(mkErr2(tp), mkErr3(enc, meth))
}
