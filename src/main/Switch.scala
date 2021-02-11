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
  def tests: List[TestFile] = List(TestFile("Switch", TestList(list1)))
  def list1 = for (switch <- List(M2P, P2M); call <- List(Meth, Prop)) yield switchFile(switch, call)

  sealed trait Switch; case object M2P extends Switch; case object P2M extends Switch

  def switchFile(switch: Switch, call: MethOrProp): TestFile = {
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

    def overrideMsg(sv: SV, sev: Sev) = (switch, sv, sev) match {
      case (M2P, S2, _) => Msg(sev, s"method without a parameter list overrides a method with a single empty one")
      case (M2P, S3, W) => Msg(sev, s"error overriding method $meth in $encl of type (): $tp;\n  method $meth of type => $tp no longer has compatible type")
      case (M2P, S3, E) => Msg(sev, s"error overriding method $meth in $encl of type (): $tp;\n  method $meth of type => $tp has incompatible type")

      case (P2M, S2, W) => Msg(sev, s"method with a single empty parameter list overrides method without any parameter list")
      case (P2M, S2, E) => Msg(sev, s"method with a single empty parameter list overrides method without any parameter list\ndef $meth: $tp (defined in $encl)")
      case (P2M, S3, W) => Msg(sev, s"error overriding method $meth in $encl of type => $tp;\n  method $meth of type (): $tp no longer has compatible type")
      case (P2M, S3, E) => Msg(sev, s"error overriding method $meth in $encl of type => $tp;\n  method $meth of type (): $tp has incompatible type")
    }

    def autoAppMsg(sv: SV, sev: Sev) = (call, switch, sv) match {
      case (Meth,   _,  _) => None
      case (Prop,   _, S2) => Some(Msg(  W, autoApp2(meth.value)))
      case (Prop, M2P, S3) => None
      case (Prop, P2M, S3) => Some(Msg(sev, autoApp3(meth.value)))
    }

    val msgs = multi3 { (sv, sev) =>
      autoAppMsg(sv, sev) match {
        case Some(msg @ Msg(E, _)) => List(msg)
        case autoAppMsg            => overrideMsg(sv, sev) :: autoAppMsg.toList
      }
    }

    TestFile(name, TestContents(List(tdefn, cdefn, vdefn), List(List(stat)), msgs))
  }
}
