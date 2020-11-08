package shisa
package testdata

import scala.meta._

import Severity.{ Info, Warn, Error }

object Switch {
  import Call._, ErrorMsgs._

  def tests         = switchTests ::: switchVcTests
  def switchTests   = for (switch <- List(Meth2Prop, Prop2Meth); call <- List(Meth, Prop)) yield switchFile(switch, call)
  def switchVcTests = for (switch <- List(Meth2Prop, Prop2Meth); call <- List(Meth, Prop)) yield switchFile(switch, call, true)

  sealed trait MethPropSwitch; case object Meth2Prop extends MethPropSwitch; case object Prop2Meth extends MethPropSwitch
  sealed trait MethOrProp;     case object Meth      extends MethOrProp;     case object Prop      extends MethOrProp
  sealed trait SV;             case object S2        extends SV;             case object S3        extends SV
  sealed trait WorE;           case object W         extends WorE;           case object E         extends WorE

  object MethPropSwitch {
    implicit class Ops(private val switch: MethPropSwitch) extends AnyVal {
      def clsDefn: Defn.Class = switch match {
        case Meth2Prop => q"""class M2P(val x: String) extends M { def d   = "" }"""
        case Prop2Meth => q"""class P2M(val x: String) extends P { def d() = "" }"""
      }
      def traitDefn: Defn.Trait = switch match {
        case Meth2Prop => q"trait M extends Any { def d() : String }"
        case Prop2Meth => q"trait P extends Any { def d   : String }"
      }
      def valDefn: Defn.Val = switch match {
        case Meth2Prop => q"""val m2p = new M2P("")"""
        case Prop2Meth => q"""val p2m = new P2M("")"""
      }
      def toStat(call: MethOrProp): Term = call match {
        case Prop => valDefn match { case Defn.Val(_, List(Pat.Var(name)), _, _) => Term.Select(name, q"d") }
        case Meth => Term.Apply(toStat(Prop), Nil)
      }
    }
  }

  object WorE {
    implicit class Ops(private val wore: WorE) extends AnyVal {
      def toSev: Severity = wore match { case W => Warn case E => Error }
    }
  }

  def overrideM(sv: SV, switch: MethPropSwitch, wore: WorE, traitName: String) = (sv, switch) match {
    case (S2, Meth2Prop) => msg(wore.toSev, 3, override2_meth2prop)
    case (S3, Meth2Prop) => msg(wore.toSev, 3, override3_meth2prop(wore.toSev, traitName))
    case (S2, Prop2Meth) => msg(wore.toSev, 3, override2_prop2meth(wore.toSev, traitName))
    case (S3, Prop2Meth) => msg(wore.toSev, 3, override3_prop2meth(wore.toSev, traitName))
  }

  def switchMsgs(switch: MethPropSwitch, call: MethOrProp, sv: SV, wore: WorE, traitName: String) = (switch, call, sv, wore) match {
    case (_,         Meth, _, _)  => List(overrideM(sv, switch, wore, traitName))

    case (Meth2Prop, Prop, S2, _) => List(overrideM(sv, switch, wore, traitName), warn(5, autoApp2("d")))
    case (Meth2Prop, Prop, S3, _) => List(overrideM(sv, switch, wore, traitName))

    case (Prop2Meth, Prop, S2, _) => List(overrideM(sv, switch, wore, traitName), msg(Warn,  5, autoApp2("d")))
    case (Prop2Meth, Prop, S3, W) => List(overrideM(sv, switch, wore, traitName), msg(Warn,  5, autoApp3("d")))
    case (Prop2Meth, Prop, S3, E) => List(                                        msg(Error, 5, autoApp3("d")))
  }

  def switchFile(switch: MethPropSwitch, call: MethOrProp, isVC: Boolean = false): TestFile = {
    val pre = switch match { case Meth2Prop => "m2p" case Prop2Meth => "p2m" }
    val suf = call   match { case Meth      => "m"   case Prop      => "p"   }
    val name = if (isVC) s"switch_vc/${pre}_$suf" else s"switch/${pre}_$suf"
    val clsDefn = if (isVC) switch.clsDefn.toValueClass else switch.clsDefn
    val msgs = multi3(switchMsgs(switch, call, _, _, switch.traitDefn.name.value))
    TestFile(name, TestContents(List(switch.traitDefn, clsDefn, switch.valDefn), List(List(switch.toStat(call))), msgs))
  }
}
