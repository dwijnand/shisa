package shisa

import scala.meta._, contrib._, Transformations._
import nme._, tpnme._

// things that matter:
// * call `##` as a nilary method (`x.##()`)
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
  def tests: List[TestFile] = methPTest :: propMTest :: negTests :: posTests :: Nil

  def mkV(name: Term.Name, tp: Type.Name) = q"val ${name.asPat}: $tp = ${Lit.String("")}"
  val vals = List(mkV(q"any", tpnme.Any), mkV(q"ref", tpnme.AnyRef), mkV(q"obj", tpnme.Object), mkV(q"str", tpnme.String))
  val vcV1 = param"val x: String"

  def methPTest  = TestFile("Call.meth_p", Test(q"def foo() = 1", q"foo",   autoApp(q"object Test", q"foo")))
  def propMTest  = TestFile("Call.prop_m", Test(q"def foo   = 2", q"foo()", noParams("object Test", q"foo", tpnme.Int)))
  def   negTests = TestFile("Call.neg",    for (x <- vals) yield mkNegNilTest(x, x.inst, nme.hashHash))
  def   posTests = TestFile("Call.posVal", List(
    for (               defn <- vals) yield mkPosNulTest(defn, defn.inst, nme.hashHash),
    for (meth <- meths; defn <- vals; test <- mkPosTests(defn, defn.inst, meth)) yield test,
    for (defn <- clss;                test <- mkPosTests(defn, defn.inst, nme.toString_)) yield test,
    for (defn <- clsR;                test <- mkPosTests(defn, defn.inst, nme.run      )) yield test,
  ).flatten)

  def mkPosNilTest(defn: Defn, inst: Term, meth: Term.Name) = Test(defn, nil(inst, meth), Msgs())
  def mkPosNulTest(defn: Defn, inst: Term, meth: Term.Name) = Test(defn, nul(inst, meth), Msgs())
  def mkPosTests  (defn: Defn, inst: Term, meth: Term.Name) = List(mkPosNulTest(defn, inst, meth), mkPosNilTest(defn, inst, meth))
  def mkNegNilTest(defn: Defn, inst: Term, meth: Term.Name) = Test(defn, nil(inst, meth), noParams("class Any", meth, tpnme.Int))

  def   CR = q"class   CR".withRunnable
  def  CCR = q"class  CCR".withRunnable.toCaseClass
  def  VCR = q"class  VCR".toValueClass(vcV1)
  def VCCR = q"class VCCR".toValueClass(vcV1).toCaseClass

  def clsR = List(CR, CCR)
  def clss = (clsR ::: List(VCR, VCCR)).flatMap { cls =>
    val s = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "S")).append[Stat](q"override def ${nme.toString_}   = ${Lit.String("")}")
    val j = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "J")).append[Stat](q"override def ${nme.toString_}() = ${Lit.String("")}")
    List(cls, s, j)
  }
  def meths = List(nme.toString_, nme.getClass_, nme.hashCode_)

  def nul(qual: Term, name: Term.Name): Term       = q"$qual.$name"
  def nil(qual: Term, name: Term.Name): Term       = q"$qual.$name()"

  def noParams(enc: String, meth: Term.Name, tp: Type.Name) = (
        Msgs.for2(_ => Msg(E, s"$tp does not take parameters"))
    ::: Msgs.for3(_ => Msg(E, s"method $meth in $enc does not take parameters"))
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
