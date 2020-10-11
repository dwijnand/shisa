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

  // TODO: Abstract over problem/line-number

  object hashHash extends MkInMemoryTestFile {
    val path              = Paths.get("testdata/Call.##.scala")
    //val contentss       = for (v <- vals; stat <- duo(v.name, q"##")) yield TestContents(Nil, List(v.defn), List(List(stat)))
    val contentss         = vals.map(v => TestContents(Nil, List(v.defn), List(duo(v.name,  q"##"))))
    override val contents = contentss.reduce(_ ++ _)
    val outerDefns        = contents.outerDefns
    val innerDefns        = contents.innerDefns
    val testStats         = contents.testStats
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
  }
}
