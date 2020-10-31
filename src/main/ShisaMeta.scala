package scala.meta

import scala.language.implicitConversions

import java.lang.System.{ lineSeparator => EOL }

import scala.meta.prettyprinters.{ Show, Syntax }
import scala.meta.prettyprinters.Show.{ newline => n, repeat => r, sequence => s }

import shisa._

object ShisaMeta {
  def testFileSource(contents: TestContents, pkgName: Option[Term.Ref] = None): String = {
    val TestContents(outerDefns, baseClass, innerDefns, testStatss, _) = contents
    val statss = if (innerDefns.isEmpty) testStatss else innerDefns :: testStatss
    val body   = r(statss, EOL)(Show(stats => r(stats.map(i(_)))))
    val cls    = baseClass match {
      case Some(base) => s(s"class Test extends ${base.name} {", body, n("}"))
      case None       => s("class Test {", body, n("}"))
    }

    val outerDefnss = if (baseClass.isEmpty) outerDefns else outerDefns :+ baseClass.toList
    val outer =
      if (outerDefnss.isEmpty) Show.None
      else s(r(outerDefnss, EOL + EOL)(Show(defns => r(defns, EOL)(syntax[Defn]))), EOL, EOL)

    pkgName match {
      case Some(name) => s("package", " ", name.syntax, EOL, EOL, outer, cls, EOL).toString
      case None       => s(                                       outer, cls, EOL).toString
    }
  }

  def i[A: Syntax](x: A)                          = Show.indent(x)
  def r2[A: Syntax](x: List[A], sep: String = "") = Show.repeat(x, sep)

  implicit def syntaxMods: Syntax[List[Mod]]           = Syntax(mods    => if (mods.isEmpty)    s() else r2(mods, " "))
  implicit def syntaxTparams: Syntax[List[Type.Param]] = Syntax(tparams => if (tparams.isEmpty) s() else s("[", r2(tparams, ", "), "]"))

  def show[A](implicit z: Show[A]): Show[A]       = z
  def syntax[A](implicit z: Syntax[A]): Syntax[A] = z
}
