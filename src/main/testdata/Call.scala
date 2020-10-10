package shisa
package testdata

import java.nio.file._

import scala.meta._

final case class Val(name: Term.Name, tpe: Type.Name) {
  def defn = q"""val ${Pat.Var(name)}: $tpe = """""
}

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

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addStat(stat: Stat) = cls.copy(templ = cls.templ.copy(stats = cls.templ.stats :+ stat))
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = init :: cls.templ.inits))
    def chNamePre(ch: Char) = cls.copy(name = cls.name.copy(value = cls.name.value.stripSuffix("CR") + s"${ch}CR"))

    def toCaseClass = cls.chNamePre('C').copy(
      mods = cls.mods :+ Mod.Case(),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss.onNil(List(Nil), idF).map(_.map(_.notValParam))),
    )

    def toValueClass = cls.chNamePre('V').addInit(init"AnyVal").copy(
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil           => List(List(param"val x: String"))
        case List(List(p)) => List(List(p.toValParam))
        case paramss       => sys.error(s"Can't toValueClass ${cls.name} b/c of paramss: $paramss")
      }),
    )

    def withRunnable = addInit(init"Runnable").addStat(q"def run() = ()")
  }

  val vals = List(Val(q"any", t"Any"), Val(q"ref", t"AnyRef"), Val(q"obj", t"Object"), Val(q"str", t"String"))

  def duo(qual: Term, name: Term.Name) = List(q"$qual.$name", q"$qual.$name()")

  object hashHash extends MkInMemoryTestFile {
    val path              = Paths.get("testdata/Call.##.scala")
    val hashHash          = q"##"
    val contentss         = vals.map(v => TestContents(Nil, List(v.defn), List(duo(v.name, hashHash))))
    override val contents = contentss.reduce(_ ++ _)
    val outerPrelude      = contents.outerPrelude
    val innerPrelude      = contents.innerPrelude
    val testStats         = contents.testStats
  }

  object pos extends MkInMemoryTestFile {
    val path = Paths.get("testdata/Call.pos.scala")

    def alt(t: Term, suff: Char) = t match {
      case q"new ${n @ Type.Name(_)}(...$argss)" => q"new ${n.chSuff(suff)}(...$argss)"
      case q"${n @ Term.Name(_)}(..$args)"       => q"${n.chSuff(suff)}(..$args)"
    }

    def toStrings(r: Term)       = duo(r, q"toString") ::: duo(alt(r, 'S'), q"toString") ::: duo(alt(r, 'J'), q"toString")
    def toStringsAndRun(r: Term) = duo(r, q"run") ::: toStrings(r)

    def classesList(cls: Defn.Class)  = List(
      cls,
      cls.copy(name = cls.name.chSuff('S')).addStat(q"""override def toString  = """""),
      cls.copy(name = cls.name.chSuff('J')).addStat(q"""override def toString() = """""),
    )

    def classesLists(cls: Defn.Class) = List(classesList(cls), classesList(cls.toCaseClass))

    val outerPrelude = classesLists(q"class CR".withRunnable) ::: classesLists(q"class CR".toValueClass)
    val innerPrelude = vals.map(_.defn)

    val testStats = vals.map(_.name).map { nme =>
        duo(nme, q"getClass") ::: duo(nme, q"hashCode") ::: duo(nme, q"toString")
      } :::
        List(toStringsAndRun(q"new CR()"))  :::
        List(toStrings(q"""new VCR("")""")) :::
        List(toStringsAndRun(q"CCR()"))     :::
        List(toStrings(q"""VCCR("")"""))
  }
}
