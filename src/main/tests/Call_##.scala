package shisa
package tests

import scala.language.implicitConversions

import java.nio.file._

import scala.meta._

object Call_## {
  val testFile = InMemoryTestFile(
    Paths.get("tests/Call.##.scala"),
    outerPrelude = Nil,
    innerPrelude = List[Defn](
      Defn.Val(Nil, List(Pat.Var(Term.Name("any"))), Some(Type.Name("Any")),    Lit.String("")),
      Defn.Val(Nil, List(Pat.Var(Term.Name("ref"))), Some(Type.Name("AnyRef")), Lit.String("")),
    ),
    testStats = List[List[Stat]](
      List[Stat](
        Term.Select(Term.Name("any"), Term.Name("##")),
        Term.Apply(Term.Select(Term.Name("any"), Term.Name("##")), Nil),
      ),
      List[Stat](
        Term.Select(Term.Name("ref"), Term.Name("##")),
        Term.Apply(Term.Select(Term.Name("ref"), Term.Name("##")), Nil),
      ),
    )
  )
}
