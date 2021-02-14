package shisa

import scala.meta._, contrib._

// in order:
// override method switching from meth to prop, and call both ways
// things that don't matter:
// * scala/java-defined
// * receiver type (Any, AnyRef, Object, String)
// * receiver mods: value class, case class
// * receiver name, super name
// * method name/result type (hashCode, toString, getClass)
object Switch {
  def tests: List[TestFile] = List(TestFile("Switch", TestList(list1)), TestFile("Switch.only", TestList(list0)))
  def list1 = for (switch <- List(M2P, P2M); call <- List(Meth, Prop)) yield new SwitchFile(switch, call).switchAndCallTestFile()
  def list0 = for (switch <- List(M2P, P2M); call <- List(Meth, Prop)) yield new SwitchFile(switch, call).justSwitchTestFile()

  sealed trait Switch; case object M2P extends Switch; case object P2M extends Switch

  class SwitchFile(switch: Switch, call: MethOrProp) {
    val pref  = switch match { case M2P  => "M2P" case P2M  => "P2M" }
    val suff  = call   match { case Meth => "M"   case Prop => "P"   }
    val name  = s"${pref}_$suff"
    val meth  = Term.Name(name.toLowerCase)
    val tp    = tpnme.String
    val value = Lit.String("")
    val mdecl = switch match { case M2P  => q"def $meth(): $tp"   case P2M => q"def $meth: $tp" }
    val mdefn = switch match { case M2P  => q"def $meth = $value" case P2M => q"def $meth() = $value" }
    val tname = Type.Name(s"Foo_$name")
    val cname = Type.Name(s"Bar_$name")
    val vname = Term.Name(s"qux_$meth")
    val tdefn = q"trait $tname extends Any { $mdecl }"
    val cdefn = q"class $cname(val x: String) extends $tname() { $mdefn }"
    val vdefn = q"val ${vname.asPat} = new $cname(${Lit.String("")})"
    val stat  = call   match { case Meth => q"$vname.$meth()" case Prop => q"$vname.$meth" }
    val encl  = s"trait $tname"

    def overrideMsgM2P = mkOverrideMsgs(
      OverrideMsg(W, "method without a parameter list overrides a method with a single empty one"),
      OverrideMsg(E, "method without a parameter list overrides a method with a single empty one"),
      overrideMsg3(W, s"(): $tp", s"=> $tp"),
      overrideMsg3(E, s"(): $tp", s"=> $tp"),
    )

    def overrideMsgP2M = mkOverrideMsgs(
      OverrideMsg(W, s"method with a single empty parameter list overrides method without any parameter list"),
      OverrideMsg(E, s"method with a single empty parameter list overrides method without any parameter list\ndef $meth: $tp (defined in $encl)"),
      overrideMsg3(W, s"=> $tp", s"(): $tp"),
      overrideMsg3(E, s"=> $tp", s"(): $tp"),
    )

    def overrideMsg3(sev: Sev, pt: String, tp: String) = sev match {
      case W => OverrideMsg(W, s"error overriding method $meth in $encl of type $pt;\n  method $meth of type $tp no longer has compatible type")
      case E => OverrideMsg(E, s"error overriding method $meth in $encl of type $pt;\n  method $meth of type $tp has incompatible type")
    }

    def mkOverrideMsgs(msg2w: Msg, msg2e: Msg, msg3w: Msg, msg3e: Msg) =
      Msgs(List(msg2w), List(msg2e), List(msg3w), List(msg3e), List(msg3e), List(msg3e))

    def switchAutoApp(switch: Switch, call: MethOrProp) = (switch, call) match {
      case (_, Meth) => noMsgs // no auto-application if calling as meth, in either m2p or p2m
      case (M2P, _)  => autoApp(meth).for2 // m2p_p is only auto-application for S2
      case _         => autoApp(meth)
    }

    val autoAppMsgs  = switchAutoApp(switch, call)
    val overrideMsgs = switch match { case M2P => overrideMsgM2P case P2M => overrideMsgP2M }

    def justSwitchTestFile() = TestFile(name, mkTest(tdefn, cdefn, overrideMsgs))

    def switchAndCallTestFile() = {
      val msgs = autoAppMsgs ++ overrideMsgs // auto-app errors suppress override messages
      TestFile(name, TestContents(List(tdefn, cdefn, vdefn), List(stat), msgs))
    }
  }
}
