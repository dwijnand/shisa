package shisa

import scala.meta._, contrib._
import Transformations._, nme._, tpnme._

// things that matter:
// * call `##` as a nilary method (`x.##()`)
// * switch calling
// things that don't matter:
// * defined as nullary vs nilary (when called as such)
// things that don't matter (removed):
// * receiver type (Any, AnyRef, Object, String)
// * with receiver vs on implied `this` receiver
// * value class, case class
// * method name (hashCode, getClass, run)
// * method result tpe (int, Class[?], void)
// * java-defined (run)
// todo matter?
// * defined as enriched method
// todo don't matter:
// * defn + call in separate vs joint compilation
// * the enclosing: method, nesting, constructors
// * other compiler settings
object Call {
  sealed trait Call; case object Meth extends Call; case object Prop extends Call

  val allTests = List(
    mkTest(q"m", q"class M { def m() = 1 }",                   q"class M",   q"m",        Prop)(Neg),
    mkTest(q"p", q"class P { def p   = 1 }",                   q"class P",   q"p",        Meth)(Neg),
    mkTest(q"a", q"class A",                                   q"class Any", q"##",       Meth)(Neg),
    mkTest(q"a", q"class A",                                   q"class Any", q"toString", Prop)(Pos),
    mkTest(q"s", q"class S { override def toString   = $NS }", q"class Any", q"toString", Prop)(Pos),
    mkTest(q"s", q"class S { override def toString   = $NS }", q"class Any", q"toString", Meth)(Pos),
    mkTest(q"j", q"class J { override def toString() = $NS }", q"class Any", q"toString", Prop)(Pos),
  )

  def mkTest(qual: Term.Name, defn: Defn.Class, encl: Defn, meth: Term.Name, call: Call)(res: Res) = {
    val defns = List(defn, q"val ${qual.asPat} = new ${defn.name}()")
    val stat  = call match { case Prop => q"$qual.$meth"      case Meth => q"$qual.$meth()"          }
    val msg0  = call match { case Prop => autoApp(encl, meth) case Meth => noParams(encl, meth, Int) }
    val msgs  = res  match { case Pos  => Msgs()              case Neg  => msg0                      }
    TestContents(defns, List(stat), msgs)
  }

  def noParams(encl: Defn, meth: Term.Name, tp: Type.Name) = (
        Msgs.for2(_ => Msg(E, s"$tp does not take parameters"))
    ::: Msgs.for3(_ => Msg(E, s"method $meth in $encl does not take parameters"))
  )

  val (posTests, negTests) = allTests.partition(_.msgs.isEmpty)
  val tests                = List(TestFile("Call.pos", posTests), TestFile("Call.neg", negTests))
}
