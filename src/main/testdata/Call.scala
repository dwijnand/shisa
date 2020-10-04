package shisa
package testdata

import java.nio.file._

import scala.meta._

final case class Val(name: Term.Name, tpe: Type.Name) {
  def defn = q"""val ${Pat.Var(name)}: $tpe = """""
}

object Call {
  val vals     = List(Val(q"any", t"Any"), Val(q"ref", t"AnyRef"), Val(q"obj", t"Object"), Val(q"str", t"String"))
  val valDefns = vals.map(_.defn)
  val valNames = vals.map(_.name)

  implicit class NameOps[N <: Name](private val name: N) extends AnyVal {
    def altName(suff: Char): N = name match {
      case n @ Term.Name(v) => n.copy(value = v.dropRight(1) + suff).asInstanceOf[N]
      case n @ Type.Name(v) => n.copy(value = v.dropRight(1) + suff).asInstanceOf[N]
    }
  }

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def toCaseClass: Defn.Class = cls.copy(
      mods = List(Mod.Case()),
      name = Type.Name(cls.name.value.stripSuffix("CR") + "CCR"),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil => List(Nil) // `case class Foo()` not `case class Foo`
        case xss => xss.map(_.map(p => p.copy(mods = p.mods.filter(_.isNot[Mod.ValParam]))))
      }),
    )
  }

  def duo(qual: Term, name: Term.Name) = List(q"$qual.$name", q"$qual.$name()")

  object hashHash extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/Call.##.scala")
    val outerPrelude = Nil
    val innerPrelude = valDefns
    val testStats    = valNames.map(duo(_, q"##"))
  }

  object pos extends MkInMemoryTestFile {
    val path = Paths.get("testdata/Call.pos.scala")

    val cr  = q"""class  CR extends Runnable { def run() = () }"""
    val vcr = q"""class VCR(val x: String) extends AnyVal"""

    def classAlt(cls: Defn.Class, suff: Char, paramss: List[List[Term.Param]]) = cls.copy(
      name = cls.name.altName(suff),
      templ = cls.templ.copy(
        inits = cls.templ.inits.filter { case Init(Type.Name(n), _, _) => n != "Runnable" },
        stats = List(q"""override def toString(...$paramss) = """""),
      )
    )

    def classesList(cls: Defn.Class)  = List(cls, classAlt(cls, 'S', Nil), classAlt(cls, 'J', List(Nil)))
    def classesLists(cls: Defn.Class) = List(classesList(cls), classesList(cls.toCaseClass))

    def alt(t: Term, suff: Char) = t match {
      case q"new ${n @ Type.Name(_)}(...$argss)" => q"new ${n.altName(suff)}(...$argss)"
      case q"${n @ Term.Name(_)}(..$args)"       => q"${n.altName(suff)}(..$args)"
    }

    def toStrings(r: Term)       = duo(r, q"toString") ::: duo(alt(r, 'S'), q"toString") ::: duo(alt(r, 'J'), q"toString")
    def toStringsAndRun(r: Term) = duo(r, q"run") ::: toStrings(r)

    val outerPrelude = classesLists(cr) ::: classesLists(vcr)
    val innerPrelude = valDefns

    val testStats = valNames.map { nme =>
        duo(nme, q"getClass") ::: duo(nme, q"hashCode") ::: duo(nme, q"toString")
      } :::
        List(toStringsAndRun(q"new CR()"))  :::
        List(toStrings(q"""new VCR("")""")) :::
        List(toStringsAndRun(q"CCR()"))     :::
        List(toStrings(q"""VCCR("")"""))
  }
}
