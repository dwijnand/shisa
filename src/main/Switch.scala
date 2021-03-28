package shisa

import scala.meta._, contrib._
import nme._, tpnme._

// in order:
// override method switching from meth to prop, and call both ways
// things that don't matter:
// * scala/java-defined
// * receiver type (Any, AnyRef, Object, String)
// * receiver mods: value class, case class
// * receiver name, super name
// * method name/result type (hashCode, toString, getClass)
object Switch {
  import Call.{ Call, Meth, Prop }
  sealed trait Switch; case object M2P extends Switch; case object P2M extends Switch

  class SwitchFile(switch: Switch, call: Call) {
    val pref  = switch match { case M2P  => "M2P" case P2M  => "P2M" }
    val suff  = call   match { case Meth => "M"   case Prop => "P"   }
    val name  = s"${pref}_$suff"
    val meth  = Term.Name(name.toLowerCase)
    val tname = Type.Name(s"Foo_$name")
    val cname = Type.Name(s"Bar_$name")
    val vname = Term.Name(s"qux_$meth")
    val mdecl = switch match { case M2P  => q"def $meth(): $String" case P2M  => q"def $meth: $String" }
    val mdefn = switch match { case M2P  => q"def $meth = $NS"      case P2M  => q"def $meth() = $NS"  }
    val stat  = call   match { case Meth => q"$vname.$meth()"       case Prop => q"$vname.$meth"      }
    val tdefn = q"trait $tname extends Any { $mdecl }"
    val cdefn = q"class $cname(val x: String) extends $tname() { $mdefn }"
    val vdefn = q"val ${vname.asPat} = new $cname($NS)"
    val encl  = s"trait $tname"

    val autoAppMsgs  = (switch, call) match {
      case (_, Meth) => Msgs() // no auto-application if calling as meth, in either m2p or p2m
      case (M2P, _)  => autoApp(q"class $cname", meth).for2 // m2p_p is only auto-application for S2
      case _         => autoApp(q"class $cname", meth)
    }

    val overrideMsgs = switch match {
      case M2P => mkOverrideMsgs(
        Msg(W, "method without a parameter list overrides a method with a single empty one"),
        Msg(E, "method without a parameter list overrides a method with a single empty one"),
        Msg(W, s"error overriding method $meth in $encl of type (): $String;\n  method $meth of type => $String no longer has compatible type"),
        Msg(E, s"error overriding method $meth in $encl of type (): $String;\n  method $meth of type => $String has incompatible type"),
      )
      case P2M => mkOverrideMsgs(
        Msg(W, s"method with a single empty parameter list overrides method without any parameter list"),
        Msg(E, s"method with a single empty parameter list overrides method without any parameter list\ndef $meth: $String (defined in $encl)"),
        Msg(W, s"error overriding method $meth in $encl of type => $String;\n  method $meth of type (): $String no longer has compatible type"),
        Msg(E, s"error overriding method $meth in $encl of type => $String;\n  method $meth of type (): $String has incompatible type"),
      )
    }

    def mkOverrideMsgs(msg2w: Msg, msg2e: Msg, msg3w: Msg, msg3e: Msg) =
      Msgs(List(msg2w), List(msg2e), List(msg3w), List(msg3e), List(msg3e), List(msg3e))

    val switchAndCallMsgs = autoAppMsgs ++ overrideMsgs // auto-app errors suppress override messages

    val    justSwitch = TestContents(List(tdefn),               List(cdefn),     overrideMsgs)
    val switchAndCall = TestContents(List(tdefn, cdefn, vdefn), List(stat), switchAndCallMsgs)
  }

  val files = for (switch <- List(M2P, P2M); call <- List(Meth, Prop)) yield new SwitchFile(switch, call)
  val tests = List(
    TestFile("Switch.only", TestList(for (f <- files) yield TestFile(f.name, f.justSwitch))),
    TestFile("Switch",      TestList(for (f <- files) yield TestFile(f.name, f.switchAndCall))),
  )
}
