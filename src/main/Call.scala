package shisa

import scala.meta._, classifiers.{ Classifiable, Classifier }, contrib._

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
// * value class or case class
// * method name or method result tpe (hashCode, toString, getClass)
// todo matter?
// * defined as enriched method
// todo don't matter:
// * separate/joint compilation
// * java-defined
// * generalise enclosing: method, nesting, constructors
// * other settings
object Call {
  def tests: List[TestFile] = methP_Test :: propM_Test :: negValTests :: posValTests :: clsTests :: Nil

  val methP_Test = TestFile("Call.meth_p", Test(q"def foo() = 1", q"foo",   autoApp(q"object Test", q"foo")))
  val propM_Test = TestFile("Call.prop_m", Test(q"def foo   = 2", q"foo()", noParams("object Test", q"foo", tpnme.Int)))

  val   CR = q"class   CR".withRunnable
  val  CCR = q"class  CCR".withRunnable.toCaseClass
  val  VCR = q"class  VCR".toValueClass
  val VCCR = q"class VCCR".toValueClass.toCaseClass

  val clsR = List(CR, CCR)
  val clss = (clsR ::: List(VCR, VCCR)).flatMap { cls =>
    val s = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "S")).addStat(q"override def ${nme.toString_}   = ${Lit.String("")}")
    val j = cls.copy(name = Type.Name(cls.name.value.stripSuffix("R") + "J")).addStat(q"override def ${nme.toString_}() = ${Lit.String("")}")
    List(cls, s, j)
  }
  def mkV(name: Term.Name, tp: Type.Name) = q"val ${name.asPat}: $tp = ${Lit.String("")}"
  val vals  = List(mkV(q"any", tpnme.Any), mkV(q"ref", tpnme.AnyRef), mkV(q"obj", tpnme.Object), mkV(q"str", tpnme.String))
  val meths = List(nme.toString_, nme.getClass_, nme.hashCode_)

  def nul(qual: Term, name: Term.Name): Term       = q"$qual.$name"
  def nil(qual: Term, name: Term.Name): Term       = q"$qual.$name()"
  def two(qual: Term, name: Term.Name): List[Term] = List(nul(qual, name), nil(qual, name))

  def mkNegTest(defn: Defn, inst: Term, meth: Term.Name) = Test(defn, nil(inst, meth), noParams("class Any", meth, tpnme.Int))
  def mkPosTest(defn: Defn, inst: Term, meth: Term.Name) = for (stat <- two(inst, meth)) yield Test(defn, stat, Msgs())

  val negValTests = TestFile("Call.negVal", for (x <- vals) yield mkNegTest(x, x.inst, nme.hashHash))

  val posValTests = TestFile("Call.posVal", List(
    for (               val1 <- vals) yield Test(val1, nul(val1.inst, nme.hashHash), Msgs()),
    for (meth <- meths; val1 <- vals; test <- mkPosTest(val1, val1.inst, meth)) yield test,
  ).flatten)

  val clsTests = TestFile("Call.cls", List(
    for (x <- clss; test <- mkPosTest(x, x.inst, nme.toString_)) yield test,
    for (x <- clsR; test <- mkPosTest(x, x.inst, nme.run      )) yield test,
  ).flatten)

  def noParams(enc: String, meth: Term.Name, tp: Type.Name) = (
        Msgs.for2(_ => Msg(E, s"$tp does not take parameters"))
    ::: Msgs.for3(_ => Msg(E, s"method $meth in $enc does not take parameters"))
  )

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

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = param.copy(mods = param.mods.appendOnce(Mod.ValParam()))
  }

  implicit class ExtensionAdders[A](a: A) {
    def addStat(b: Stat)(implicit E: Extract[A, Stat], R: Replace[A, Stat]): A =
      R.replace(a, E.extract(a) :+ b)
  }

  implicit class ListOps[T](private val xs: List[T]) extends AnyVal {
    def has[U](implicit classifier: Classifier[T, U]): Boolean = xs.exists( classifier(_))

    def  appendOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else xs :+ x
    def prependOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else x :: xs
  }

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
}
