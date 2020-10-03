package shisa
package testdata

import scala.language.implicitConversions

import java.nio.file._

import scala.meta._

object Call_pos {
  val path = Paths.get("testdata/Call.pos.scala")

  val memberVariants = List(
    q"any" -> t"Any",
    q"ref" -> t"AnyRef",
    q"obj" -> t"Object",
    q"str" -> t"String",
  )

  val  cr  = q"""new CR()"""
  val  cs  = q"""new CS()"""
  val  cj  = q"""new CJ()"""

  val  vcr = q"""new VCR("")"""
  val  vcs = q"""new VCS("")"""
  val  vcj = q"""new VCJ("")"""

  val  ccr = q"""CCR()"""
  val  ccs = q"""CCS()"""
  val  ccj = q"""CCJ()"""

  val vccr = q"""VCCR("")"""
  val vccs = q"""VCCS("")"""
  val vccj = q"""VCCJ("")"""

  val outerPrelude = List(
    List(
      q"""class CR extends Runnable { def run() = () }""",
      q"""class CS { override def toString   = "" }""",
      q"""class CJ { override def toString() = "" }""",
    ),
    List(
      q"""case class CCR() extends Runnable { def run() = () }""",
      q"""case class CCS() { override def toString   = "" }""",
      q"""case class CCJ() { override def toString() = "" }""",
    ),
    List(
      q"""class VCR(val x: String) extends AnyVal""",
      q"""class VCS(val x: String) extends AnyVal { override def toString   = "" }""",
      q"""class VCJ(val x: String) extends AnyVal { override def toString() = "" }""",
    ),
    List(
      q"""case class VCCR(x: String) extends AnyVal""",
      q"""case class VCCS(x: String) extends AnyVal { override def toString   = "" }""",
      q"""case class VCCJ(x: String) extends AnyVal { override def toString() = "" }""",
    ),
  )

  val innerPrelude = memberVariants.map { case (nme, tpe) =>
    Defn.Val(Nil, List(Pat.Var(nme)), Some(tpe), Lit.String(""))
  }

  val testStats = memberVariants.map { case (nme, _) => (
    Term.Select(nme, q"##")
     :: invokeBothWays(nme, q"getClass")
    ::: invokeBothWays(nme, q"hashCode")
    ::: invokeBothWays(nme, q"toString")
  )} ::: List(
    invokeBothWays(cr, q"run")
    ::: invokeBothWays(cr, q"toString")
    ::: invokeBothWays(cs, q"toString")
    ::: invokeBothWays(cj, q"toString")
  ) ::: List(
        invokeBothWays(vcr, q"toString")
    ::: invokeBothWays(vcs, q"toString")
    ::: invokeBothWays(vcj, q"toString")
  )::: List(
    invokeBothWays(ccr, q"run")
    ::: invokeBothWays(ccr, q"toString")
    ::: invokeBothWays(ccs, q"toString")
    ::: invokeBothWays(ccj, q"toString")
  ) ::: List(
    invokeBothWays(vccr, q"toString")
    ::: invokeBothWays(vccs, q"toString")
    ::: invokeBothWays(vccj, q"toString")
  )

  def invokeBothWays(qual: Term, name: Term.Name) = {
    val sel = Term.Select(qual, name)
    List(sel, Term.Apply(sel, Nil))
  }

  val testFile = InMemoryTestFile(path, outerPrelude, innerPrelude, testStats)
}
