package shisa
package testdata

import scala.meta._

import Severity.{ Info, Warn, Error }

object Types {
  sealed trait SV;   case object S2  extends  SV; case object S3  extends  SV
  sealed trait BSV;  case object S2X extends BSV; case object S30 extends BSV; case object S31 extends BSV
  sealed trait WorE; case object W  extends WorE; case object E  extends WorE

  object WorE {
    implicit class Ops(private val wore: WorE) extends AnyVal {
      def toSev: Severity = wore match { case W => Warn case E => Error }
    }
  }

  def multi(msg2: Msg, msg3: Msg) =
    List(List(msg2), List(msg2), List(msg3), List(msg3), List(msg3), List(msg3))

  def multi3(msgs: (SV, WorE) => List[Msg]) =
    List(msgs(S2, W), msgs(S2, E), msgs(S3, W), msgs(S3, E), msgs(S3, E), msgs(S3, E))

  def multi4(msgs2: WorE => List[Msg], msgs30: WorE => List[Msg], msgs31: WorE => List[Msg]) =
    List(msgs2(W), msgs2(E), msgs30(W), msgs30(E), msgs31(W), msgs31(E))

  def multi5(msgs: (BSV, WorE) => List[Msg]) =
    List(msgs(S2X, W), msgs(S2X, E), msgs(S30, W), msgs(S30, E), msgs(S31, W), msgs(S31, E))
}

object Switch {
  import Call._, ErrorMsgs._, Types._

  def tests = List(call_meth_p, call_prop_m) ::: switchTests

  def switchTests = List(
    switchFile(Meth2Prop, Meth, t"Foo_M2P_M",    t"Bar_M2P_M",    q"m2p_m",    q"qux_m2p_m"),
    switchFile(Meth2Prop, Prop, t"Foo_M2P_P",    t"Bar_M2P_P",    q"m2p_p",    q"qux_m2p_p"),
    switchFile(Prop2Meth, Meth, t"Foo_P2M_M",    t"Bar_P2M_M",    q"p2m_m",    q"qux_p2m_m"),
    switchFile(Prop2Meth, Prop, t"Foo_P2M_P",    t"Bar_P2M_P",    q"p2m_P",    q"qux_p2m_p"),
    switchFile(Meth2Prop, Meth, t"Foo_M2P_M_VC", t"Bar_M2P_M_VC", q"m2p_m_vc", q"qux_m2p_m_vc", true),
    switchFile(Meth2Prop, Prop, t"Foo_M2P_P_VC", t"Bar_M2P_P_VC", q"m2p_p_vc", q"qux_m2p_p_vc", true),
    switchFile(Prop2Meth, Meth, t"Foo_P2M_M_VC", t"Bar_P2M_M_VC", q"p2m_m_vc", q"qux_p2m_m_vc", true),
    switchFile(Prop2Meth, Prop, t"Foo_P2M_P_VC", t"Bar_P2M_P_VC", q"p2m_p_vc", q"qux_p2m_p_vc", true),
  )

  def CallMethP(meth: Term.Name, value: Lit) = {
    val msgs = multi3 {
      case (S2,    _) => List(msg(Warn,       3, autoApp2(meth.value)))
      case (S3, wore) => List(msg(wore.toSev, 3, autoApp3(meth.value)))
    }
    val contents = TestContents(List(q"def $meth() = $value"), List(List(meth)), msgs)
    TestFile("Call.meth_p", contents)
  }

  def CallPropM(meth: Term.Name, value: Lit) = {
    val contents = TestContents(List(q"def $meth = $value"), List(List(q"$meth()")), multi(anyErr(3), anyErr(3)))
    TestFile("Call.prop_m", contents)
  }

  val call_meth_p = CallMethP(q"foo", q"1")
  val call_prop_m = CallPropM(q"bar", q"1")

  sealed trait MethPropSwitch; case object Meth2Prop extends MethPropSwitch; case object Prop2Meth extends MethPropSwitch
  sealed trait MethOrProp;     case object Meth      extends MethOrProp;     case object Prop      extends MethOrProp

  object MethPropSwitch {
    implicit class Ops(private val switch: MethPropSwitch) extends AnyVal {
      def clsDefn(clsName: Type.Name, traitName: Type.Name, meth: Term.Name): Defn.Class = switch match {
        case Meth2Prop => q"""class $clsName(val x: String) extends $traitName() { def $meth   = "" }"""
        case Prop2Meth => q"""class $clsName(val x: String) extends $traitName() { def $meth() = "" }"""
      }
      def traitDefn(traitName: Type.Name, meth: Term.Name): Defn.Trait = switch match {
        case Meth2Prop => q"trait $traitName extends Any { def $meth() : String }"
        case Prop2Meth => q"trait $traitName extends Any { def $meth   : String }"
      }
      def valDefn(valName: Term.Name, clsName: Type.Name): Defn.Val = switch match {
        case Meth2Prop => q"""val ${Pat.Var(valName)} = new $clsName("")"""
        case Prop2Meth => q"""val ${Pat.Var(valName)} = new $clsName("")"""
      }
      def toStat(call: MethOrProp, valName: Term.Name, meth: Term.Name): Term = call match {
        case Prop => Term.Select(valName, meth)
        case Meth => Term.Apply(switch.toStat(Prop, valName, meth), Nil)
      }
    }
  }

  val methStr = "(): String"
  val propStr = "=> String"

  def override2_meth2prop = "method without a parameter list overrides a method with a single empty one"
  def override2_prop2meth(wore: WorE, nme: String) = wore match {
    case W => s"method with a single empty parameter list overrides method without any parameter list"
    case E => s"method with a single empty parameter list overrides method without any parameter list\ndef d: String (defined in trait $nme)"
  }

  def override3_meth2prop(wore: WorE, nme: String) = override3(wore, nme, methStr, propStr)
  def override3_prop2meth(wore: WorE, nme: String) = override3(wore, nme, propStr, methStr)
  def override3(wore: WorE, nme: String, pt: String, tp: String) = wore match {
    case W => s"error overriding method d in trait $nme of type $pt;\n  method d of type $tp no longer has compatible type"
    case E => s"error overriding method d in trait $nme of type $pt;\n  method d of type $tp has incompatible type"
  }

  def overrideM(sv: SV, switch: MethPropSwitch, wore: WorE, traitName: String) = (sv, switch) match {
    case (S2, Meth2Prop) => msg(wore.toSev, 3, override2_meth2prop)
    case (S3, Meth2Prop) => msg(wore.toSev, 3, override3_meth2prop(wore, traitName))
    case (S2, Prop2Meth) => msg(wore.toSev, 3, override2_prop2meth(wore, traitName))
    case (S3, Prop2Meth) => msg(wore.toSev, 3, override3_prop2meth(wore, traitName))
  }

  def switchMsgs(switch: MethPropSwitch, call: MethOrProp, sv: SV, wore: WorE, traitName: String, meth: Term.Name) = (switch, call, sv, wore) match {
    case (_,         Meth, _, _)  => List(overrideM(sv, switch, wore, traitName))

    case (Meth2Prop, Prop, S2, _) => List(overrideM(sv, switch, wore, traitName), warn(5, autoApp2(meth.value)))
    case (Meth2Prop, Prop, S3, _) => List(overrideM(sv, switch, wore, traitName))

    case (Prop2Meth, Prop, S2, _) => List(overrideM(sv, switch, wore, traitName), msg(Warn,  5, autoApp2(meth.value)))
    case (Prop2Meth, Prop, S3, W) => List(overrideM(sv, switch, wore, traitName), msg(Warn,  5, autoApp3(meth.value)))
    case (Prop2Meth, Prop, S3, E) => List(                                        msg(Error, 5, autoApp3(meth.value)))
  }

  def switchFile(switch: MethPropSwitch, call: MethOrProp, clsName: Type.Name, traitName: Type.Name, meth: Term.Name, valName: Term.Name, isVC: Boolean = false): TestFile = {
    val pre = switch match { case Meth2Prop => "m2p" case Prop2Meth => "p2m" }
    val suf = call   match { case Meth      => "m"   case Prop      => "p"   }
    val name = if (isVC) s"switch_vc/${pre}_$suf" else s"switch/${pre}_$suf"
    val clsDefn0  = switch.clsDefn(clsName, traitName, meth)
    val clsDefn   = if (isVC) clsDefn0.toValueClass else clsDefn0
    val traitDefn = switch.traitDefn(traitName, meth)
    val msgs = multi3(switchMsgs(switch, call, _, _, traitDefn.name.value, meth))
    TestFile(name, TestContents(List(traitDefn, clsDefn, switch.valDefn(valName, clsName)), List(List(switch.toStat(call, valName, meth))), msgs))
  }
}
