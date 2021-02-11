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
// override rules:
// * in S2,                        overriding method as a property -> override s2 m2p warning
// * in S2, -Xsource:3,            overriding method as a property -> override s2 m2p error
// * in S2,                        overriding property as a method -> override s2 p2m warning
// * in S2, -Xsource:3,            overriding property as a method -> override s2 p2m error w/ addendum
// * in S3, -source 3.0-migration, overriding method as a property -> override warning 3
// * in S3,                        overriding method as a property -> override error   3
// * in S3, -source 3.0-migration, overriding property as a method -> override warning 4
// * in S3,                        overriding property as a method -> override error   4
// auto-application rules:
// *                               calling     as a method   -> no message
// * in S2,                        calling     as a property -> auto app warning 1
// * in S3,                        calling m2p as a property -> no message
// * in S3, -source 3.0-migration, calling p2m as a property -> auto app warning
// * in S3, otherwise,             calling p2m as a property -> override warning
object Switch {
  def tests: List[TestFile] = List(TestFile("Switch", TestList(list1)))
  def list1 = for (switch <- List(M2P, P2M); call <- List(Meth, Prop)) yield switchFile(switch, call)

  def switchFile(switch: Switch, call: MethOrProp): TestFile = {
    val pref  = switch match { case M2P  => "M2P" case P2M  => "P2M" }
    val suff  = call   match { case Meth => "M"   case Prop => "P"   }
    val name  = s"${pref}_$suff"
    val meth  = Term.Name(name.toLowerCase)
    val tp    = tpnme.String
    val mdecl = switch match { case M2P  => q"def $meth(): $tp" case P2M => q"def $meth: $tp" }
    val mdefn = switch match { case M2P  => q"def $meth = $ns"  case P2M => q"def $meth() = $ns" }
    val tname = Type.Name(s"Foo_$name")
    val cname = Type.Name(s"Bar_$name")
    val vname = Term.Name(s"qux_$meth")
    val tdefn = q"trait $tname extends Any { $mdecl }"
    val cdefn = q"class $cname(val x: String) extends $tname() { $mdefn }"
    val vdefn = q"val ${vname.asPat} = new $cname($ns)"
    val stat  = call   match { case Meth => q"$vname.$meth()" case Prop => q"$vname.$meth" }
    val msgs  = multi3(switchMsgs(switch, call, _, _, s"trait ${tdefn.name}", meth, tp))
    TestFile(name, TestContents(List(tdefn, cdefn, vdefn), List(List(stat)), msgs))
  }

  sealed trait Switch; case object M2P extends Switch; case object P2M extends Switch

  def nul(qual: Term, name: Term.Name): Term = q"$qual.$name"
  def nil(qual: Term, name: Term.Name): Term = q"$qual.$name()"

  // Errors
  def override2_m2p   = "method without a parameter list overrides a method with a single empty one"
  def override2_p2m   = "method with a single empty parameter list overrides method without any parameter list"
  def override2_p2m_e = (enc: String, meth: Term.Name, tp: Type) => s"\ndef $meth: $tp (defined in $enc)"

  def override3_1     = (enc: String, meth: Term.Name, pt: String) => s"error overriding method $meth in $enc of type $pt;"
  def override3_2_w   = (meth: Term.Name, tp: String)              => s"\n  method $meth of type $tp no longer has compatible type"
  def override3_2_e   = (meth: Term.Name, tp: String)              => s"\n  method $meth of type $tp has incompatible type"
  def override3_w     = (enc: String, meth: Term.Name, pt: String, tp: String) => override3_1(enc, meth, pt) + override3_2_w(meth, tp)
  def override3_e     = (enc: String, meth: Term.Name, pt: String, tp: String) => override3_1(enc, meth, pt) + override3_2_e(meth, tp)

  def override3(sev: Sev, enc: String, meth: Term.Name, pt: String, tp: String) = {
    val status = sev match { case W => "no longer has compatible type" case E => "has incompatible type" }
    s"error overriding method $meth in $enc of type $pt;\n  method $meth of type $tp $status"
  }

  def overrideMsg(switch: Switch, call: MethOrProp, sv: SV, sev: Sev, enc: String, meth: Term.Name, tp: Type) = (switch, call, sv, sev) match {
    case (M2P,    _, S2, W) => Some(warn(override2_m2p))
    case (M2P,    _, S2, E) => Some( err(override2_m2p))
    case (P2M,    _, S2, W) => Some(warn(override2_p2m))
    case (P2M,    _, S2, E) => Some( err(override2_p2m + override2_p2m_e(enc, meth, tp)))
    case (M2P,    _, S3, W) => Some(warn(override3(sev, enc, meth, s"(): $tp", s"=> $tp")))
    case (M2P,    _, S3, E) => Some( err(override3(sev, enc, meth, s"(): $tp", s"=> $tp")))
    case (P2M,    _, S3, W) => Some(warn(override3(sev, enc, meth, s"=> $tp", s"(): $tp")))
    case (P2M, Meth, S3, E) => Some( err(override3(sev, enc, meth, s"=> $tp", s"(): $tp")))
    case (P2M,    _, S3, E) => None
  }

  def autoAppMsg(switch: Switch, call: MethOrProp, sv: SV, sev: Sev, meth: Term.Name) = (switch, call, sv) match {
    case (_,   Meth,  _) => None
    case (_,   Prop, S2) => Some(warn(autoApp2(meth.value)))
    case (M2P, Prop, S3) => None
    case (P2M, Prop, S3) => Some(Msg(sev, autoApp3(meth.value)))
  }

  def switchMsgs(switch: Switch, call: MethOrProp, sv: SV, sev: Sev, enc: String, meth: Term.Name, tp: Type.Name) = {
    overrideMsg(switch, call, sv, sev, enc, meth, tp).toList ::: autoAppMsg(switch, call, sv, sev, meth).toList
  }
}
