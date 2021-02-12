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

  // TODO: test just override without calling
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
      case (M2P, S2, W) => Msg(W, s"method without a parameter list overrides a method with a single empty one")
      case (M2P, S2, E) => Msg(E, s"method without a parameter list overrides a method with a single empty one")
      case (P2M, S2, W) => Msg(W, s"method with a single empty parameter list overrides method without any parameter list")
      case (P2M, S2, E) => Msg(E, s"method with a single empty parameter list overrides method without any parameter list\ndef $meth: $tp (defined in $encl)")

      case (M2P, S3, W) => Msg(W, s"error overriding method $meth in $encl of type (): $tp;\n  method $meth of type => $tp no longer has compatible type")
      case (M2P, S3, E) => Msg(E, s"error overriding method $meth in $encl of type (): $tp;\n  method $meth of type => $tp has incompatible type")
      case (P2M, S3, W) => Msg(W, s"error overriding method $meth in $encl of type => $tp;\n  method $meth of type (): $tp no longer has compatible type")
      case (P2M, S3, E) => Msg(E, s"error overriding method $meth in $encl of type => $tp;\n  method $meth of type (): $tp has incompatible type")
    }

    val msgs = {
      def go(f: (SV, Sev) => List[Msg]) =
        Msgs(f(S2, W), f(S2, E), f(S3, W), f(S3, E), f(S3, E), f(S3, E))
      go((sv, sev) => (call, sv, switch, sev) match {
        case (Meth,  _,   _, _) => List(overrideMsg(sv, sev))
        case (   _, S2,   _, _) => List(overrideMsg(sv, sev), Msg(  W, autoApp2(meth.value)))
        case (   _,  _, M2P, _) => List(overrideMsg(sv, sev))
        case (   _,  _,   _, W) => List(overrideMsg(sv, sev), Msg(sev, autoApp3(meth.value)))
        case _                  => List(                      Msg(sev, autoApp3(meth.value)))
      })
    }

    TestFile(name, TestContents(List(tdefn, cdefn, vdefn), List(stat), msgs))
  }
}
