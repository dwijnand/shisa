package shisa
package testdata

import scala.meta._, classifiers.{ Classifiable, Classifier }

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

  implicit class ListOps[T](private val xs: List[T]) extends AnyVal {
    def has[U](implicit classifier: Classifier[T, U]): Boolean    = xs.exists(classifier(_))
    def hasNot[U](implicit classifier: Classifier[T, U]): Boolean = xs.forall(!classifier(_))

    def appendOnce[U](x: T)(implicit classifier: Classifier[T, U])  = if (xs.has[U]) xs else xs :+ x
    def prependOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else x :: xs
  }

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = param.copy(mods = param.mods.appendOnce(Mod.ValParam()))
  }

  val initAnyVal = init"AnyVal"

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addStat(stat: Stat) = cls.copy(templ = cls.templ.copy(stats = cls.templ.stats :+ stat))
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = cls.templ.inits.prependOnce(init)))

    def toCaseClass = cls.copy(
      mods = cls.mods.appendOnce(Mod.Case()),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil     => List(Nil)
        case paramss => paramss.map(_.map(_.notValParam))
      }),
    )

    def toValueClass = cls.addInit(initAnyVal).copy(
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil           => List(List(param"val x: String"))
        case List(List(p)) => List(List(p.toValParam))
        case paramss       => sys.error(s"Can't toValueClass ${cls.name} b/c of paramss: $paramss")
      }),
    )

    def withRunnable = addInit(init"Runnable").addStat(q"def run() = ()")

    def  name: Type.Name = cls.name
    def tname: Term.Name = Term.Name(name.value)

    def inst: Term = cls match {
      case _ if !cls.mods.has[Mod.Case] && !cls.templ.inits.contains(initAnyVal) => q"new $name()"
      case _ if  cls.mods.has[Mod.Case] && !cls.templ.inits.contains(initAnyVal) => q"$tname()"
      case _ if !cls.mods.has[Mod.Case] &&  cls.templ.inits.contains(initAnyVal) => q"new $name($ns)"
      case _ if  cls.mods.has[Mod.Case] &&  cls.templ.inits.contains(initAnyVal) => q"$tname($ns)"
    }
  }

  sealed trait ClassVariant
  object ClassVariant { case object Case extends ClassVariant; case object Value extends ClassVariant; case object Runnable extends ClassVariant }

  final case class Cls(variants: List[ClassVariant], suffix: String = "R") {
    import ClassVariant._

    val name: Type.Name = variants.foldRight(Type.Name(s"C$suffix")) {
      case (Case,     name) => name.copy("C" + name.value)
      case (Value,    name) => name.copy("V" + name.value)
      case (Runnable, name) => name
    }
    val tname: Term.Name = Term.Name(name.value)

    val defn: Defn.Class = variants.foldLeft(q"class $name") {
      case (cls, Case)     => cls.toCaseClass
      case (cls, Value)    => cls.toValueClass
      case (cls, Runnable) => cls.withRunnable
    }

    val inst: Term = defn.inst

    lazy val copyS = copy(suffix = "S")
    lazy val copyJ = copy(suffix = "J")
    lazy val defnS = copyS.defn.addStat(q"override def toString   = $ns")
    lazy val defnJ = copyJ.defn.addStat(q"override def toString() = $ns")
    lazy val defns = List(defn, defnS, defnJ)
  }

  val any  = Val(q"any", t"Any")
  val ref  = Val(q"ref", t"AnyRef")
  val obj  = Val(q"obj", t"Object")
  val str  = Val(q"str", t"String")
  val vals = List(any, ref, obj, str)
  val   CR = Cls(List(ClassVariant.Runnable))
  val  CCR = Cls(List(ClassVariant.Runnable, ClassVariant.Case))
  val  VCR = Cls(List(ClassVariant.Value))
  val VCCR = Cls(List(ClassVariant.Value, ClassVariant.Case))
  val cls1 = List(CR, CCR, VCR, VCCR)
  val clss = cls1.flatMap(cls => List(cls, cls.copyS, cls.copyJ))

  final case class Val(name: Term.Name, tpe: Type.Name) {
    val defn = Defn.Val(Nil, List(Pat.Var(name)), Option(tpe), Lit.String(""))
  }

  object hashHash extends MkInMemoryTestFile {
    val name     = "Call.##"
    val err2     = err(                   "Int does not take parameters")
    val err3     = err("method ## in class Any does not take parameters")
    val contents = vals.map(v => TestContents(List(v.defn), List(List(q"${v.name}.##()")), multi(err2, err3))).reduce(_ ++ _).toUnit
  }

  object pos extends MkInMemoryTestFile {
    val name  = "Call.pos"
    val defns = cls1.flatMap(_.defns) ::: vals.map(_.defn)
    val stats =
      vals.map          { v   => List(q"${v.name}.##")                                    } :::
      vals.map          { v   => List(q"${v.name}.toString",   q"${v.name}.toString()")   } :::
      vals.map          { v   => List(q"${v.name}.getClass",   q"${v.name}.getClass()")   } :::
      vals.map          { v   => List(q"${v.name}.hashCode",   q"${v.name}.hashCode()")   } :::
      clss.map          { cls => List(q"${cls.inst}.toString", q"${cls.inst}.toString()") } :::
      List(CR, CCR).map { cls => List(q"${cls.inst}.run",      q"${cls.inst}.run()")      }
    def contents = TestContents(defns, stats, noMsgs).toUnit
  }
}
