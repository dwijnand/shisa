package shisa
package testdata

import scala.language.implicitConversions

import java.nio.file._

import scala.meta._

object Call_pos extends MkInMemoryTestFile {
  val path = Paths.get("testdata/Call.pos.scala")

  val vals = List(
    q"any" -> t"Any",
    q"ref" -> t"AnyRef",
    q"obj" -> t"Object",
    q"str" -> t"String",
  )

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
  val innerPrelude = vals.map { case (nme, tpe) => val patVar = Pat.Var(nme); q"""val $patVar: $tpe = """"" }

  val testStats =
    vals.map { case (nme, _) =>
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
