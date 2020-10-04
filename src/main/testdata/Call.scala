package shisa
package testdata

import java.nio.file._

import scala.meta._

final case class Val(name: Term.Name, tpe: Type.Name) {
  def defn = q"""val ${Pat.Var(name)}: $tpe = """""
}

object Call {
  object vals {
    val any = Val(q"any", t"Any")
    val ref = Val(q"ref", t"AnyRef")
    val obj = Val(q"obj", t"Object")
    val str = Val(q"str", t"String")
  }

  object hashHash extends MkInMemoryTestFile {
    val path = Paths.get("testdata/Call.##.scala")

    val variants = List(Call.vals.any, Call.vals.ref)

    val outerPrelude = Nil

    val innerPrelude = variants.map(_.defn)
    val testStats    = variants.map { case Val(nme, _) => List(q"$nme.##", q"$nme.##()") }
  }

  object pos extends MkInMemoryTestFile {
    val path = Paths.get("testdata/Call.pos.scala")

    val vals = List(Call.vals.any, Call.vals.ref, Call.vals.obj, Call.vals.str)

    val cr  = q"""class  CR extends Runnable { def run() = () }"""
    val vcr = q"""class VCR(val x: String) extends AnyVal"""

    def classAlt(cls: Defn.Class, suff: Char, paramss: List[List[Term.Param]]) = cls.copy(
      name = Type.Name(cls.name.value.dropRight(1) + suff),
      templ = cls.templ.copy(
        inits = cls.templ.inits.filter { case Init(Type.Name(n), _, _) => n != "Runnable" },
        stats = List(q"""override def toString(...$paramss) = """""),
      )
    )

    def makeCaseClass(cls: Defn.Class) = cls.copy(
      mods = List(Mod.Case()),
      name = Type.Name(cls.name.value.stripSuffix("CR") + "CCR"),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil => List(Nil)
        case xss => xss.map(_.map(param => param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))))
      }),
    )

    def classesList(cls: Defn.Class)  = List(cls, classAlt(cls, 'S', Nil), classAlt(cls, 'J', List(Nil)))
    def classesLists(cls: Defn.Class) = List(classesList(cls), classesList(makeCaseClass(cls)))

    val outerPrelude = classesLists(cr) ::: classesLists(vcr)
    val innerPrelude = vals.map(_.defn)

    val testStats =
      vals.map { case Val(nme, _) =>
        q"$nme.##" :: duo(nme, q"getClass") ::: duo(nme, q"hashCode") ::: duo(nme, q"toString")
      } :::
          List(toStringsAndRun(q"new CR()")) :::
          List(toStrings(q"""new VCR("")""")) :::
          List(toStringsAndRun(q"CCR()")) :::
          List(toStrings(q"""VCCR("")"""))

    def duo(qual: Term, name: Term.Name) = List(q"$qual.$name", q"$qual.$name()")

    def alt(t: Term, suff: Char) = t match {
      case q"new ${Name(name)}(...$argss)" => q"new ${Type.Name(name.dropRight(1) + suff)}(...$argss)"
      case q"${Name(name)}(..$args)"       => q"${Term.Name(name.dropRight(1) + suff)}(..$args)"
    }

    def toStrings(r: Term)       = duo(r, q"toString") ::: duo(alt(r, 'S'), q"toString") ::: duo(alt(r, 'J'), q"toString")
    def toStringsAndRun(r: Term) = duo(r, q"run") ::: toStrings(r)
  }
}
