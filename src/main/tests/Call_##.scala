package shisa
package tests

import scala.language.implicitConversions

import java.nio.file._

import scala.meta._

object Call_## {
  val path = Paths.get("tests/Call.##.scala")

  val hashHash = q"##"
  val variants = List(q"any" -> t"Any", q"ref" -> t"AnyRef")

  val innerPrelude = variants.map { case (nme, tpe) =>
    Defn.Val(Nil, List(Pat.Var(nme)), Some(tpe), Lit.String(""))
  }

  val testStats = variants.map { case (nme, _) =>
    val sel = Term.Select(nme, hashHash)
    List(sel, Term.Apply(sel, Nil))
  }

  val testFile = InMemoryTestFile(path, outerPrelude = Nil, innerPrelude, testStats)
}
