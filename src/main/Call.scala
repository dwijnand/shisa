package shisa

import scala.meta._, contrib._, Transformations._
import nme._, tpnme._

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

  val a = List(q"class A { def m() = 1 ; def p = 2 }",         q"val a = new A()")
  val s = List(q"class S { override def $toString_   = $NS }", q"val s = new S()")
  val j = List(q"class J { override def $toString_() = $NS }", q"val j = new J()")

  val allTests = List(
    mkTest(a, q"class A",   q"m",      Prop)(Neg),
    mkTest(a, q"class A",   q"p",      Meth)(Neg),
    mkTest(a, q"class Any", toString_, Prop)(Pos),
    mkTest(a, q"class Any", hashHash,  Meth)(Neg),
    mkTest(s, q"class Any", toString_, Prop)(Pos),
    mkTest(s, q"class Any", toString_, Meth)(Pos),
    mkTest(j, q"class Any", toString_, Prop)(Pos),
  )

  def mkTest(defns: List[Defn], encl: Defn, meth: Term.Name, call: Call)(res: Res) = {
    val qual  = defns.lastOption.map(valName).getOrElse(Null)
    val stat  = call  match { case Prop => q"$qual.$meth"      case Meth => q"$qual.$meth()"          }
    val msg0  = call  match { case Prop => autoApp(encl, meth) case Meth => noParams(encl, meth, Int) }
    val msgs  = res   match { case Pos  => Msgs()              case Neg  => msg0                      }
    TestContents(defns, List(stat), msgs)
  }

  def termName(term: Term.Ref): Term.Name = term match {
    case Term.Select(_, name) => name
    case t @ Term.Name(_)     => t
    case _                    => Term.Name("<N/A>")
  }

  def valName(defn: Defn): Term = defn match {
    case Defn.Val(_, List(Pat.Var(name)), _, _) => name
    case _                                      => Null
  }

  def defnName(defn: Defn): Tree = defn match {
    case defn: Member.Type                      => defnTypeName(defn)
    case _                                      => defnTermName(defn)
  }
  def defnTypeName(defn: Defn with Member.Type): Type.Name = defn.name
  def defnTermName(defn: Defn): Term = defn match {
    case Defn.Val(_, List(Pat.Var(name)), _, _) => name
    case Defn.Var(_, List(Pat.Var(name)), _, _) => name
    case Defn.Given(_, name, _, _, _)           => name.asTerm
    case Defn.GivenAlias(_, name, _, _, _, _)   => name.asTerm
    case defn: Member.Term                      => defn.name
    case _                                      => Null
  }

  def noParams(encl: Defn, meth: Term.Name, tp: Type.Name) = (
        Msgs.for2(_ => Msg(E, s"$tp does not take parameters"))
    ::: Msgs.for3(_ => Msg(E, s"method $meth in $encl does not take parameters"))
  )

  val (posTests, negTests) = allTests.partition(_.msgs.isEmpty)
  val tests                = List(TestFile("Call.pos", posTests), TestFile("Call.neg", negTests))
}
