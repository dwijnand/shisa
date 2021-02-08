package shisa

import scala.meta._

object Switch {
  def tests: List[TestFile] = call_meth_p :: call_prop_m :: switchTests

  val call_meth_p = CallMethP(q"foo", q"1")
  val call_prop_m = CallPropM(q"bar", q"1")
  def switchTests = List(
    TestFile("Switch", TestList(List(
      switchFile(M2P, Meth, t"Foo_M2P_M",    t"Bar_M2P_M",    q"m2p_m",    q"qux_m2p_m"),
      switchFile(M2P, Prop, t"Foo_M2P_P",    t"Bar_M2P_P",    q"m2p_p",    q"qux_m2p_p"),
      switchFile(P2M, Meth, t"Foo_P2M_M",    t"Bar_P2M_M",    q"p2m_m",    q"qux_p2m_m"),
      switchFile(P2M, Prop, t"Foo_P2M_P",    t"Bar_P2M_P",    q"p2m_P",    q"qux_p2m_p"),
    ))),
    TestFile("Switch_vc", TestList(List(
      switchFile(M2P, Meth, t"Foo_M2P_M_VC", t"Bar_M2P_M_VC", q"m2p_m_vc", q"qux_m2p_m_vc", true),
      switchFile(M2P, Prop, t"Foo_M2P_P_VC", t"Bar_M2P_P_VC", q"m2p_p_vc", q"qux_m2p_p_vc", true),
      switchFile(P2M, Meth, t"Foo_P2M_M_VC", t"Bar_P2M_M_VC", q"p2m_m_vc", q"qux_p2m_m_vc", true),
      switchFile(P2M, Prop, t"Foo_P2M_P_VC", t"Bar_P2M_P_VC", q"p2m_p_vc", q"qux_p2m_p_vc", true),
    ))),
  )

  def CallMethP(meth: Term.Name, value: Lit) = {
    val msgs = multi3 {
      case (S2,   _) => List(msg(W,   autoApp2(meth.value)))
      case (S3, sev) => List(msg(sev, autoApp3(meth.value)))
    }
    TestFile("Call.meth_p", TestContents(List(q"def $meth() = $value"), List(List(q"$meth")), msgs))
  }

  def CallPropM(meth: Term.Name, value: Lit) = {
    val msgs = multi(anyErr, anyErr)
    TestFile("Call.prop_m", TestContents(List(q"def $meth = $value"), List(List(q"$meth()")), msgs))
  }

  def switchFile(switch: MethPropSwitch, call: MethOrProp,
                 clsName: Type.Name, traitName: Type.Name, meth: Term.Name, valName: Term.Name,
                 isVC: Boolean = false): TestFile = {
    val pre = switch match { case M2P  => "m2p" case P2M  => "p2m" }
    val suf = call   match { case Meth => "m"   case Prop => "p"   }
    val name = if (isVC) s"Switch_vc/${pre}_$suf" else s"Switch/${pre}_$suf"
    val clsDefn0  = switch.clsDefn(clsName, traitName, meth)
    val clsDefn   = if (isVC) clsDefn0.toValueClass else clsDefn0
    val traitDefn = switch.traitDefn(traitName, meth)
    val defns = List(traitDefn, clsDefn, switch.valDefn(valName, clsName))
    val stats = List(List(switch.toStat(call, valName, meth)))
    val msgs  = multi3(switchMsgs(switch, call, _, _, traitDefn.name.value, meth))
    TestFile(name, TestContents(defns, stats, msgs))
  }

  // Types
  sealed trait MethPropSwitch; case object M2P  extends MethPropSwitch; case object P2M  extends MethPropSwitch
  sealed trait MethOrProp;     case object Meth extends MethOrProp;     case object Prop extends MethOrProp

  object MethPropSwitch {
    implicit class Ops(private val switch: MethPropSwitch) extends AnyVal {
      def clsDefn(clsName: Type.Name, traitName: Type.Name, meth: Term.Name): Defn.Class = switch match {
        case M2P => q"class $clsName(val x: String) extends $traitName() { def $meth   = $ns }"
        case P2M => q"class $clsName(val x: String) extends $traitName() { def $meth() = $ns }"
      }
      def traitDefn(traitName: Type.Name, meth: Term.Name): Defn.Trait = switch match {
        case M2P => q"trait $traitName extends Any { def $meth() : String }"
        case P2M => q"trait $traitName extends Any { def $meth   : String }"
      }
      def valDefn(valName: Term.Name, clsName: Type.Name): Defn.Val = switch match {
        case M2P => q"val ${Pat.Var(valName)} = new $clsName($ns)"
        case P2M => q"val ${Pat.Var(valName)} = new $clsName($ns)"
      }
      def toStat(call: MethOrProp, valName: Term.Name, meth: Term.Name): Term = call match {
        case Prop => q"$valName.$meth"
        case Meth => q"$valName.$meth()"
      }
    }
  }

  // Errors
  val methStr = "(): String"
  val propStr = "=> String"

  def override2_meth2prop = "method without a parameter list overrides a method with a single empty one"
  def override2_prop2meth(sev: Sev, nme: String, meth: Term.Name) = sev match {
    case W => s"method with a single empty parameter list overrides method without any parameter list"
    case E => s"method with a single empty parameter list overrides method without any parameter list\ndef $meth: String (defined in trait $nme)"
  }

  def override3_meth2prop(sev: Sev, nme: String, meth: Term.Name) = override3(sev, nme, methStr, propStr, meth)
  def override3_prop2meth(sev: Sev, nme: String, meth: Term.Name) = override3(sev, nme, propStr, methStr, meth)
  def override3(sev: Sev, nme: String, pt: String, tp: String, meth: Term.Name) = sev match {
    case W => s"error overriding method $meth in trait $nme of type $pt;\n  method $meth of type $tp no longer has compatible type"
    case E => s"error overriding method $meth in trait $nme of type $pt;\n  method $meth of type $tp has incompatible type"
  }

  def overrideM(sv: SV, switch: MethPropSwitch, sev: Sev, traitName: String, meth: Term.Name) = (sv, switch) match {
    case (S2, M2P)       => msg(sev, override2_meth2prop)
    case (S3, M2P) => msg(sev, override3_meth2prop(sev, traitName, meth))
    case (S2, P2M) => msg(sev, override2_prop2meth(sev, traitName, meth))
    case (S3, P2M) => msg(sev, override3_prop2meth(sev, traitName, meth))
  }

  def switchMsgs(switch: MethPropSwitch, call: MethOrProp, sv: SV, sev: Sev, traitName: String, meth: Term.Name) = {
    val overrideMsg = overrideM(sv, switch, sev, traitName, meth)
    (switch, call, sv, sev) match {
      case (_,         Meth, _, _)  => List(overrideMsg)

      case (M2P, Prop, S2, _) => List(overrideMsg, warn(autoApp2(meth.value)))
      case (M2P, Prop, S3, _) => List(overrideMsg)

      case (P2M, Prop, S2, _) => List(overrideMsg, msg(W, autoApp2(meth.value)))
      case (P2M, Prop, S3, W) => List(overrideMsg, msg(W, autoApp3(meth.value)))
      case (P2M, Prop, S3, E) => List(             msg(E, autoApp3(meth.value)))
    }
  }
}
