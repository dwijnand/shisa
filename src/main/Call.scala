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

  sealed trait MethKind
  final case class OldMeth(encl: Defn, meth: Term.Name) extends MethKind
  final case class NewMeth(defn: Defn.Def)              extends MethKind

  final case class QualDefns(qual: Term, defns: List[Defn])

  val str = QualDefns(q"str", List(                                                           q"""val str = ""      """))
  val cs  = QualDefns(q"cs",  List(q"""class CS { override def ${nme.toString_}   = "" }""", q"""val cs  = new CS()"""))
  val cj  = QualDefns(q"cj",  List(q"""class CJ { override def ${nme.toString_}() = "" }""", q"""val cj  = new CJ()"""))
  val nul = QualDefns(Null,   Nil)

  val allTests = List(
    mkTest(str, OldMeth(q"class  Any", nme.toString_), Prop)(Pos),
    mkTest(str, OldMeth(q"class  Any", nme.toString_), Meth)(Pos),
    mkTest(str, OldMeth(q"class  Any", nme.hashHash),  Prop)(Pos),
    mkTest(str, OldMeth(q"class  Any", nme.hashHash),  Meth)(Neg),
    mkTest(cs,  OldMeth(q"class  Any", nme.toString_), Prop)(Pos),
    mkTest(cs,  OldMeth(q"class  Any", nme.toString_), Meth)(Pos),
    mkTest(cj,  OldMeth(q"class  Any", nme.toString_), Prop)(Pos),
    mkTest(cj,  OldMeth(q"class  Any", nme.toString_), Meth)(Pos),
    mkTest(nul, NewMeth(q"def m1() = 1"),              Prop)(Neg),
    mkTest(nul, NewMeth(q"def m2   = 2"),              Meth)(Neg),
  )

  def mkTest(qualDefns: QualDefns, methKind: MethKind, call: Call)(res: Res) = {
    val QualDefns(qual, defns0) = qualDefns
    val (encl, meth) = methKind match {
      case OldMeth(encl, meth) => (encl, meth)
      case NewMeth(defn)       => (q"object Test", defn.name)
    }
    val defns = methKind match {
      case OldMeth(_, _) => defns0
      case NewMeth(defn) => defns0 :+ defn
    }
    val term  = qual match { case Null => meth                case _    => Term.Select(qual, meth)   }
    val stat  = call match { case Meth => q"$term()"          case Prop => q"$term"                  }
    val msg0  = call match { case Meth => autoApp(encl, meth) case Prop => noParams(encl, meth, Int) }
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
