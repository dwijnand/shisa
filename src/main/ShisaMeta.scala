package scala.meta

import scala.language.implicitConversions

import java.lang.System.{ lineSeparator => EOL }

import scala.meta.prettyprinters.{ Show, Syntax }
import scala.meta.prettyprinters.Show.{ newline => n, repeat => r, sequence => s }

import shisa._

object ShisaMeta {
  def show[A](implicit z: Show[A]): Show[A]       = z
  def syntax[A](implicit z: Syntax[A]): Syntax[A] = z

  def i[A: Syntax](x: A)                          = Show.indent(x)
  def r2[A: Syntax](x: List[A], sep: String = "") = Show.repeat(x, sep)

  def testFileSource(testFile: InMemoryTestFile): String = {
    val InMemoryTestFile(_, outerPrelude, innerPrelude, testStatss) = testFile
    val statss = if (innerPrelude.isEmpty) testStatss else innerPrelude :: testStatss
    val body   = r(statss, EOL)(Show(stats => r(stats.map(i(_)))))
    val cls    = s("class Test {", body, n("}"))

    val main =
      if (outerPrelude.isEmpty) cls
      else s(r(outerPrelude.map(i(_))), n(cls))

    s(main, EOL).toString
  }

  implicit def syntaxMods: Syntax[List[Mod]]           = Syntax(mods    => if (mods.isEmpty)    s() else r2(mods, " "))
  implicit def syntaxTparams: Syntax[List[Type.Param]] = Syntax(tparams => if (tparams.isEmpty) s() else s("[", r2(tparams, ", "), "]"))
}
