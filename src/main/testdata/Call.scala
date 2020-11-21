package shisa
package testdata

import scala.meta._

import Severity.{ Info, Warn, Error }

trait MkInMemoryTestFile {
  def name: String
  def contents: TestContents
  final def testFile: TestFile = TestFile(name, contents)
}

object Call {
  import ErrorMsgs._, Types._

  def tests = List(hashHash, pos).map(_.testFile)

  implicit class NameOps[N <: Name](private val name: N) extends AnyVal {
    def chSuff(ch: Char) = name match {
      case n @ Term.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
      case n @ Type.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
    }
  }

  def appendOnce[T, U](xs: List[T], x: T)(implicit classifier: classifiers.Classifier[T, U]) = {
    if (xs.exists(classifier(_))) xs else xs :+ x
  }

  def prependOnce[T, U](x: T, xs: List[T])(implicit classifier: classifiers.Classifier[T, U]) = {
    if (xs.exists(classifier(_))) xs else x :: xs
  }

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = param.copy(mods = appendOnce(param.mods, Mod.ValParam()))
  }

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addStat(stat: Stat) = cls.copy(templ = cls.templ.copy(stats = cls.templ.stats :+ stat))
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = prependOnce(init, cls.templ.inits)))

    def toCaseClass = cls.copy(
      mods = appendOnce(cls.mods, Mod.Case()),
      ctor = cls.ctor.copy(paramss = if (cls.ctor.paramss.isEmpty) List(Nil) else cls.ctor.paramss.map(_.map(_.notValParam))),
    )

    def toValueClass = cls.addInit(init"AnyVal").copy(
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil           => List(List(param"val x: String"))
        case List(List(p)) => List(List(p.toValParam))
        case paramss       => sys.error(s"Can't toValueClass ${cls.name} b/c of paramss: $paramss")
      }),
    )

    def withRunnable = addInit(init"Runnable").addStat(q"def run() = ()")
  }

  sealed trait ClassVariant
  sealed trait Override
  object ClassVariant { case object Case extends ClassVariant; case object Value extends ClassVariant; case object Runnable extends ClassVariant }
  object Override     { case object No   extends Override;     case object Java extends Override;      case object Scala    extends Override     }

  final case class Cls(variants: List[ClassVariant]) {
    val name: Type.Name = variants.foldRight(t"CR") {
      case (ClassVariant.Case,     name) => name.copy("C" + name.value)
      case (ClassVariant.Value,    name) => name.copy("V" + name.value)
      case (ClassVariant.Runnable, name) => name
    }

    val defn: Defn.Class = {
      if (variants.isEmpty) q"class $name".withRunnable
      else {
        variants.foldLeft(q"class $name") {
          case (cls, ClassVariant.Case)     => cls.toCaseClass
          case (cls, ClassVariant.Value)    => cls.toValueClass
          case (cls, ClassVariant.Runnable) => cls.withRunnable
        }
      }
    }

    val defnS = defn.copy(name = defn.name.chSuff('S')).addStat(q"override def toString   = $ns")
    val defnJ = defn.copy(name = defn.name.chSuff('J')).addStat(q"override def toString() = $ns")
    val defns = List(defn, defnS, defnJ)
  }

  val any   = Val(q"any", t"Any")
  val ref   = Val(q"ref", t"AnyRef")
  val obj   = Val(q"obj", t"Object")
  val str   = Val(q"str", t"String")
  val vals  = List(any, ref, obj, str)
  val vnmes = vals.map(_.name)

  val   CR = Cls(List(ClassVariant.Runnable))
  val  CCR = Cls(List(ClassVariant.Runnable, ClassVariant.Case))
  val  VCR = Cls(List(ClassVariant.Value))
  val VCCR = Cls(List(ClassVariant.Value, ClassVariant.Case))

  final case class Val(name: Term.Name, tpe: Type.Name) {
    val defn = Defn.Val(Nil, List(Pat.Var(name)), Option(tpe), Lit.String(""))
  }

  def duo(qual: Term, name: Term.Name) = List(List(q"$qual.$name", q"$qual.$name()"))

  def doesNotTakeParams(subject: String) = s"$subject does not take parameters"

  object hashHash extends MkInMemoryTestFile {
    val name     = "Call.##"
    val err2     = err(doesNotTakeParams("Int"))
    val err3     = err(doesNotTakeParams("method ## in class Any"))
    val contents = vals.map { v =>
      TestContents(List(v.defn), duo(v.name, q"##"), multi(err2, err3))
    }.reduce(_ ++ _).toUnit
  }

  object pos extends MkInMemoryTestFile {
    val name  = "Call.pos"
    val defns = CR.defns ::: CCR.defns ::: VCR.defns ::: VCCR.defns ::: vals.map(_.defn)

    def alt(t: Term, suff: Char) = t match {
      case q"new ${n @ Type.Name(_)}(...$argss)" => q"new ${n.chSuff(suff)}(...$argss)"
      case q"${n @ Term.Name(_)}(..$args)"       => q"${n.chSuff(suff)}(..$args)"
    }

    val toStr              = q"toString"
    def toStrings(r: Term) = duo(r, toStr) ::: duo(alt(r, 'S'), toStr) ::: duo(alt(r, 'J'), toStr)

    val stats =
      vnmes.flatMap(duo(_, q"toString")) :::
      vnmes.flatMap(duo(_, q"getClass")) :::
      vnmes.flatMap(duo(_, q"hashCode")) :::
      List(q"new CR()", q"CCR()").flatMap(r => duo(r, q"run") ::: toStrings(r)) :::
      List(q"new VCR($ns)", q"VCCR($ns)").flatMap(r => toStrings(r))

    def contents = TestContents(defns, stats, List(Nil, Nil, Nil, Nil, Nil, Nil, Nil))
  }
}
