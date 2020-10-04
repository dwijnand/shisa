package shisa
package testdata

import scala.language.implicitConversions

import java.nio.file._

import scala.meta._

object Call_## extends MkInMemoryTestFile {
  val path = Paths.get("testdata/Call.##.scala")

  val variants = List(q"any" -> t"Any", q"ref" -> t"AnyRef")

  val outerPrelude = Nil

  val innerPrelude = variants.map { case (nme, tpe) =>
    Defn.Val(Nil, List(Pat.Var(nme)), Some(tpe), Lit.String(""))
  }

  val testStats = variants.map { case (nme, _) => List(q"$nme.##", q"$nme.##()") }
}
