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

  final case class QualDefns(qual: Term, defns: List[Defn])
  val str = QualDefns(q"str", List(                                                          q"""val str = ""      """))
  val cs  = QualDefns(q"cs",  List(q"""class CS { override def ${nme.toString_}   = "" }""", q"""val cs  = new CS()"""))
  val cj  = QualDefns(q"cj",  List(q"""class CJ { override def ${nme.toString_}() = "" }""", q"""val cj  = new CJ()"""))
  val nul = QualDefns(Null,   Nil)

  sealed trait MethKind
  final case class OldMeth(encl: Defn, meth: Term.Name) extends MethKind
  final case class NewMeth(defn: Defn.Def)              extends MethKind

  val toStr = OldMeth(q"class Any", nme.toString_)
  val hashH = OldMeth(q"class Any", nme.hashHash)
  val meth1 = NewMeth(q"def m() = 1")
  val prop1 = NewMeth(q"def p   = 2")

  val allTests = List(
    mkTest(str, toStr, Prop)(Pos),
    mkTest(str, toStr, Meth)(Pos),
    mkTest(str, hashH, Prop)(Pos),
    mkTest(str, hashH, Meth)(Neg),
    mkTest(cs,  toStr, Prop)(Pos),
    mkTest(cs,  toStr, Meth)(Pos),
    mkTest(cj,  toStr, Prop)(Pos),
    mkTest(cj,  toStr, Meth)(Pos),
    mkTest(nul, meth1, Prop)(Neg),
    mkTest(nul, prop1, Meth)(Neg),
  )

  def mkTest(qualDefns: QualDefns, methKind: MethKind, call: Call)(res: Res) = {
    val QualDefns(qual, defns0) = qualDefns
    val defns = methKind match { case OldMeth(_, _)    => defns0              case NewMeth(defn) => defns0 :+ defn            }
    val encl  = methKind match { case OldMeth(encl, _) => encl                case NewMeth(_)    => q"object Test"            }
    val meth  = methKind match { case OldMeth(_, meth) => meth                case NewMeth(defn) => defn.name                 }
    val term  = qual     match { case Null             => meth                case _             => Term.Select(qual, meth)   }
    val stat  = call     match { case Prop             => q"$term"            case Meth          => q"$term()"                }
    val msg0  = call     match { case Prop             => autoApp(encl, meth) case Meth          => noParams(encl, meth, Int) }
    val msgs  = res      match { case Pos              => Msgs()              case Neg           => msg0                      }
    TestContents(defns, List(stat), msgs)
  }

  def termName(term: Term): Term.Name = term match {
    case Term.Select(_, name) => name
    case t @ Term.Name(_)     => t
    case _                    => throw new IllegalArgumentException(s"No name in $term")
  }

  def noParams(encl: Defn, meth: Term.Name, tp: Type.Name) = (
        Msgs.for2(_ => Msg(E, s"$tp does not take parameters"))
    ::: Msgs.for3(_ => Msg(E, s"method $meth in $encl does not take parameters"))
  )

  val (posTests, negTests) = allTests.partition(_.msgs.isEmpty)
  val tests                = List(TestFile("Call.pos", posTests), TestFile("Call.neg", negTests))
}
