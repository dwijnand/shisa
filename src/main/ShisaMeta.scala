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

  def testFileSource(contents: TestContents): String = {
    val TestContents(outerDefns, innerDefns, testStatss) = contents
    val statss = if (innerDefns.isEmpty) testStatss else innerDefns :: testStatss
    val body   = r(statss, EOL)(Show(stats => r(stats.map(i(_)))))
    val cls    = s("class Test {", body, n("}"))

    val outer =
      if (outerDefns.isEmpty) Show.None
      else s(r(outerDefns, EOL + EOL)(Show(defns => r(defns, EOL)(syntax[Defn]))), EOL, EOL)

    s(outer, cls, EOL).toString
  }

  implicit def syntaxMods: Syntax[List[Mod]]           = Syntax(mods    => if (mods.isEmpty)    s() else r2(mods, " "))
  implicit def syntaxTparams: Syntax[List[Type.Param]] = Syntax(tparams => if (tparams.isEmpty) s() else s("[", r2(tparams, ", "), "]"))
}
