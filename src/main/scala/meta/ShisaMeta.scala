package scala.meta

import scala.language.implicitConversions

import java.lang.System.{ lineSeparator => EOL }

import scala.meta.prettyprinters.{ Show, Syntax }

import shisa._

object ShisaMeta {
  def show[T <: Tree](implicit z: Show[T]): Show[T]       = z
  def syntax[T <: Tree](implicit z: Syntax[T]): Syntax[T] = z
  def toSyntax[T <: Tree : Syntax](t: T)                  = syntax[T].apply(t)

  def testFileSource(testFile: InMemoryTestFile): String = {
    val InMemoryTestFile(_, outerPrelude, innerPrelude, testStatss) = testFile
    val body = {
      val innerPreludeOpt = if (innerPrelude.isEmpty) Nil else List(innerPrelude)
      val ys1 :: yssN = (innerPreludeOpt ::: testStatss)
      ys1.map(toSyntax(_)) ::: yssN.flatMap {
        case s1 :: xs => Show.newline(s1)(syntax[Stat]) :: xs.map(toSyntax(_))
        case _        => Nil
      }
    }
    implicit def syntaxMods: Syntax[List[Mod]] = Syntax { mods =>
      if (mods.isEmpty) Show.sequence() else Show.repeat(mods, " ")(syntax[Mod])
    }
    implicit def syntaxTparams: Syntax[List[Type.Param]] = Syntax { tparams =>
      if (tparams.isEmpty) Show.sequence() else Show.sequence("[", Show.repeat(tparams, ", ")(syntax[Type.Param]), "]")
    }
    val tmpl = Template(Nil, Nil, Self(Name.Anonymous(), None), Nil)
    val cls = Defn.Class(Nil, Type.Name("Test"), Nil, Ctor.Primary(Nil, Type.Name("Test"), Nil), tmpl)
    val cls2 = Show.sequence(
      Show.wrap(cls.mods, " "),
      Show.sequence("class"),
      " ",
      cls.name.syntax,
      cls.tparams,
      Show.wrap(" ", cls.ctor, cls.ctor.mods.nonEmpty)(syntax[Ctor.Primary]),
      Show.sequence(" {", Show.repeat(body.map(Show.indent(_)), ""), Show.newline("}")),
    )
    val showRes =
      if (outerPrelude.isEmpty) cls2
      else Show.sequence(Show.repeat(outerPrelude, EOL)(syntax[Defn]), Show.newline(cls2))
    showRes.toString
  }
}
