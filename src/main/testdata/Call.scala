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

  def altName(name: Name, suff: Char) = name.value.dropRight(1) + suff

  def alt(t: Term, suff: Char) = t match {
    case q"new ${n @ Name(_)}(...$argss)" => q"new ${Type.Name(altName(n, suff))}(...$argss)"
    case q"${n @ Name(_)}(..$args)"       => q"${Term.Name(altName(n, suff))}(..$args)"
  }

  object hashHash extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/Call.##.scala")
    val variants     = List(Call.vals.any, Call.vals.ref)
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
      name = Type.Name(altName(cls.name, suff)),
      templ = cls.templ.copy(
        inits = cls.templ.inits.filter { case Init(Type.Name(n), _, _) => n != "Runnable" },
        stats = List(q"""override def toString(...$paramss) = """""),
      )
    )

    def makeCaseClass(cls: Defn.Class) = cls.copy(
      mods = List(Mod.Case()),
      name = Type.Name(cls.name.value.stripSuffix("CR") + "CCR"),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil => List(Nil) // `case class Foo()` not `case class Foo`
        case xss => xss.map(_.map(p => p.copy(mods = p.mods.filter(_.isNot[Mod.ValParam]))))
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

    def toStrings(r: Term)       = duo(r, q"toString") ::: duo(alt(r, 'S'), q"toString") ::: duo(alt(r, 'J'), q"toString")
    def toStringsAndRun(r: Term) = duo(r, q"run") ::: toStrings(r)
  }
}
