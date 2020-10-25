package shisa
package testdata

import java.nio.file._

import scala.meta._, contrib._

object Call {
  def idF[A]: A => A = x => x

  implicit class ListOps[A](private val xs: List[A]) extends AnyVal {
    def onNil[B](z: => B, f: List[A] => B): B = if (xs.isEmpty) z else f(xs)
  }

  implicit class NameOps[N <: Name](private val name: N) extends AnyVal {
    def chSuff(ch: Char) = name match {
      case n @ Term.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
      case n @ Type.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
    }
  }

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = param.copy(mods = param.mods :+ Mod.ValParam())
  }

  sealed trait ClassVariant
  object ClassVariant {
    case object Case     extends ClassVariant
    case object Value    extends ClassVariant
    case object Runnable extends ClassVariant
  }

  sealed trait ClassMethOverride
  object ClassMethOverride {
    case object No        extends ClassMethOverride
    case object  JavaMeth extends ClassMethOverride
    case object ScalaMeth extends ClassMethOverride
  }

  final case class Cls(variants: List[ClassVariant]) {
    val name: Type.Name = variants.foldRight(t"CR") {
      case (ClassVariant.Case,     name) => name.copy("C" + name.value)
      case (ClassVariant.Value,    name) => name.copy("V" + name.value)
      case (ClassVariant.Runnable, name) => name
    }

    val defn = {
      if (variants.isEmpty) q"class $name".withRunnable
      else {
        variants.foldLeft(q"class $name") {
          case (cls, ClassVariant.Case)     => cls.toCaseClass
          case (cls, ClassVariant.Value)    => cls.toValueClass
          case (cls, ClassVariant.Runnable) => cls.withRunnable
        }
      }
    }

    val defns = List(
      defn,
      defn.copy(name = defn.name.chSuff('S')).addStat(q"""override def toString   = """""),
      defn.copy(name = defn.name.chSuff('J')).addStat(q"""override def toString() = """""),
    )
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

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addStat(stat: Stat) = cls.copy(templ = cls.templ.copy(stats = cls.templ.stats :+ stat))
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = init :: cls.templ.inits))

    def toCaseClass = cls.copy(
      mods = cls.mods :+ Mod.Case(),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss.onNil(List(Nil), idF).map(_.map(_.notValParam))),
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

  final case class Val(name: Term.Name, tpe: Type.Name) {
    val defn = q"""val ${Pat.Var(name)}: $tpe = """""
  }

  def duo(qual: Term, name: Term.Name) = List(q"$qual.$name", q"$qual.$name()")

  val noMsgs = List(Nil, Nil, Nil, Nil, Nil, Nil, Nil)

  def multi(msg2: Msg, msg3: Msg) = List(
    List(msg2), List(msg2), List(msg2),
    List(msg3), List(msg3), List(msg3), List(msg3),
  )

  object def_meth_p extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/Call.def/Call.meth_p.scala")
    val innerDefns   = List(q"""def meth() = """"")
    val testStats    = List(List(q"meth"))
    val expectedMsgs = List(
      List(warn(4, msg2)), List(warn(4, msg2)), List(warn(4, msg2)),
      List(warn(4, msg3)), List( err(4, msg3)), List( err(4, msg3)), List(err(4, msg3)),
    )

    def msg2 = """Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method meth,
      |or remove the empty argument list from its definition (Java-defined methods are exempt).
      |In Scala 3, an unapplied method like this will be eta-expanded into a function.""".stripMargin
    def msg3 = "method meth must be called with () argument"

    def warn(lineNo: Int, str: String) = new Msg(Severity.Warning, s"target/$path", lineNo, str, "")
    def  err(lineNo: Int, str: String) = new Msg(Severity.Error,   s"target/$path", lineNo, str, "")

    def contents     = TestContents(Nil, innerDefns, testStats, expectedMsgs)
  }

  object def_prop_m extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/Call.def/Call.prop_m.scala")
    val innerDefns   = List(q"""def prop = """"")
    val testStats    = List(List(q"prop()"))
    val expectedMsgs = multi(err(4, msg2), err(4, msg3))

    def msg2 = """not enough arguments for method apply: (i: Int): Char in class StringOps.
      |Unspecified value parameter i.""".stripMargin
    def msg3 = "missing argument for parameter i of method apply: (i: Int): Char"

    def err(lineNo: Int, str: String) = new Msg(Severity.Error, s"target/$path", lineNo, str, "")

    def contents     = TestContents(Nil, innerDefns, testStats, expectedMsgs)
  }

  object hashHash extends MkInMemoryTestFile {
    val path = Paths.get("testdata/Call.##.scala")

    def errs(lineNo: Int) = multi(err2(lineNo), err3(lineNo))
    val contentss         = List(
      TestContents(Nil, List(any.defn), List(duo(any.name, q"##")), errs(8)),
      TestContents(Nil, List(ref.defn), List(duo(ref.name, q"##")), errs(11)),
      TestContents(Nil, List(obj.defn), List(duo(obj.name, q"##")), errs(14)),
      TestContents(Nil, List(str.defn), List(duo(str.name, q"##")), errs(17)),
    )
    val contents = contentss.reduce(_ ++ _)

    def err2(lineNo: Int)             = err(lineNo, "Int does not take parameters")
    def err3(lineNo: Int)             = err(lineNo, "method ## in class Any does not take parameters")
    def err(lineNo: Int, str: String) = new Msg(Severity.Error, s"target/$path", lineNo, str, "")
  }

  object pos extends MkInMemoryTestFile {
    val path = Paths.get("testdata/Call.pos.scala")

    val outerDefns = List(CR.defns, CCR.defns, VCR.defns, VCCR.defns)
    val innerDefns = vals.map(_.defn)

    def alt(t: Term, suff: Char) = t match {
      case q"new ${n @ Type.Name(_)}(...$argss)" => q"new ${n.chSuff(suff)}(...$argss)"
      case q"${n @ Term.Name(_)}(..$args)"       => q"${n.chSuff(suff)}(..$args)"
    }

    def toStrings(r: Term)       = duo(r, q"toString") ::: duo(alt(r, 'S'), q"toString") ::: duo(alt(r, 'J'), q"toString")
    def toStringsAndRun(r: Term) = duo(r, q"run") ::: toStrings(r)

    val testStats = vals.map(_.name).map { nme =>
        duo(nme, q"getClass") ::: duo(nme, q"hashCode") ::: duo(nme, q"toString")
      } :::
        List(toStringsAndRun(q"new CR()"))  :::
        List(toStrings(q"""new VCR("")""")) :::
        List(toStringsAndRun(q"CCR()"))     :::
        List(toStrings(q"""VCCR("")"""))

    def contents = TestContents(outerDefns, innerDefns, testStats, noMsgs)
  }
}
