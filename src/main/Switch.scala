package shisa

import scala.meta._, contrib._

object Switch {
  def tests: List[TestFile] = List(TestFile("Switch", TestList(list1)))
  def list1 = for (switch <- List(M2P, P2M); call <- List(Meth, Prop)) yield switchFile(switch, call)

  def switchFile(switch: Switch, call: MethOrProp): TestFile = {
    val pref  = switch match { case M2P  => "M2P" case P2M  => "P2M" }
    val suff  = call   match { case Meth => "M"   case Prop => "P"   }
    val name  = s"${pref}_$suff"
    val meth  = Term.Name(name.toLowerCase)
    val cname = Type.Name(s"Foo_$name")
    val tname = Type.Name(s"Bar_$name")
    val vname = Term.Name(s"qux_$meth")
    val mdecl = switch match { case M2P  => q"def $meth(): String" case P2M => q"def $meth: String" }
    val mdefn = switch match { case M2P  => q"def $meth = $ns"     case P2M => q"def $meth() = $ns" }
    val tdefn = q"trait $tname extends Any { $mdecl }"
    val cdefn = q"class $cname(val x: String) extends $tname() { $mdefn }"
    val vdefn = q"val ${vname.asPat} = new $cname($ns)"
    val stat  = call   match { case Meth => q"$vname.$meth()"      case Prop => q"$vname.$meth"     }
    val msgs  = multi3(switchMsgs(switch, call, _, _, tdefn.name.value, meth))
    TestFile(name, TestContents(List(tdefn, cdefn, vdefn), List(List(stat)), msgs))
  }

  sealed trait Switch; case object M2P extends Switch; case object P2M extends Switch

  // Errors
  def override2_m2p = "method without a parameter list overrides a method with a single empty one"
  def override2_p2m(sev: Sev, enc: String, meth: Term.Name) = {
    val suffix = if (sev == E) s"\ndef $meth: String (defined in $enc)" else ""
    s"method with a single empty parameter list overrides method without any parameter list$suffix"
  }

  def override3(sev: Sev, enc: String, pt: String, tp: String, meth: Term.Name) = {
    val status = sev match { case W => "no longer has compatible type" case E => "has incompatible type" }
    s"error overriding method $meth in $enc of type $pt;\n  method $meth of type $tp $status"
  }

  def overrideM(sv: SV, switch: Switch, sev: Sev, traitName: String, meth: Term.Name) = (sv, switch) match {
    case (S2, M2P) => Msg(sev, override2_m2p)
    case (S2, P2M) => Msg(sev, override2_p2m(sev, s"trait $traitName", meth))
    case (S3, M2P) => Msg(sev, override3(sev, s"trait $traitName", "(): String", "=> String", meth))
    case (S3, P2M) => Msg(sev, override3(sev, s"trait $traitName", "=> String", "(): String", meth))
  }

  def switchMsgs(switch: Switch, call: MethOrProp, sv: SV, sev: Sev, traitName: String, meth: Term.Name) = {
    (switch, call, sv, sev) match {
      case (_,   Meth, _, _)  => List(overrideM(sv, switch, sev, traitName, meth))

      case (M2P, Prop, S2, _) => List(overrideM(sv, switch, sev, traitName, meth), warn(autoApp2(meth.value)))
      case (M2P, Prop, S3, _) => List(overrideM(sv, switch, sev, traitName, meth))

      case (P2M, Prop, S2, _) => List(overrideM(sv, switch, sev, traitName, meth), warn(autoApp2(meth.value)))
      case (P2M, Prop, S3, W) => List(overrideM(sv, switch, sev, traitName, meth), warn(autoApp3(meth.value)))
      case (P2M, Prop, S3, E) => List(                                              err(autoApp3(meth.value)))
    }
  }
}
