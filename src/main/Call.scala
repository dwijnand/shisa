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
// todo matter?
// * defined as enriched method
// todo don't matter:
// * separate/joint compilation
// * java-defined
// * generalise enclosing: method, nesting, constructors
// * other settings
object Call {
  def tests: List[TestFile] = methP_Test :: propM_Test :: negValTests :: posValTests :: clsTests :: Nil

  val methP_Test = TestFile("Call.meth_p", mkTest(q"def foo() = 1", q"foo",   autoApp(q"foo")))
  val propM_Test = TestFile("Call.prop_m", mkTest(q"def foo   = 2", q"foo()", noParams("object Test", q"foo", tpnme.Int)))

  val cls1 = List(q"class  CR".withRunnable, q"class  CCR".withRunnable.toCaseClass)
  val cls2 = List(q"class VCR".toValueClass, q"class VCCR".toValueClass.toCaseClass)
  val clsV = (cls1 ::: cls2).flatMap { cls =>
    val s = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "S")).addStat(q"override def ${nme.toString_}   = ${Lit.String("")}")
    val j = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "J")).addStat(q"override def ${nme.toString_}() = ${Lit.String("")}")
    List(cls, s, j)
  }
  def mkV(name: Term.Name, tp: Type.Name) = q"val ${name.asPat}: $tp = ${Lit.String("")}"
  val vals = List(mkV(q"any", tpnme.Any), mkV(q"ref", tpnme.AnyRef), mkV(q"obj", tpnme.Object), mkV(q"str", tpnme.String))

  def nul(qual: Term, name: Term.Name): Term       = q"$qual.$name"
  def nil(qual: Term, name: Term.Name): Term       = q"$qual.$name()"
  def two(qual: Term, name: Term.Name): List[Term] = List(nul(qual, name), nil(qual, name))

  val negValTests = mkFile("Call.negVal",
    for (x <- vals; stat  = nil(x.inst, nme.hashHash )) yield mkTest(x, stat, noParams("class Any", q"##", tpnme.Int)),
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

  def noParams(enc: String, meth: Term.Name, tp: Type.Name) = {
    val subj = s"method $meth in $enc"
    msgs2or3(_ => err(s"$tp does not take parameters"), _ => err(s"$subj does not take parameters"))
  }
}
