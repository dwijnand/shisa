package shisa
package testdata

import scala.meta._

import Severity.{ Info, Warn, Error }

trait MkInMemoryTestFile {
  def name: String
  def contents: TestContents
  final def testFile: TestFile = TestFile(name, contents)
}

object Call {
  def tests         = allTests.map(_.testFile)
  def allTests      = List(hashHash, pos, def_meth_p, def_prop_m) ::: switchTests ::: switchVcTests
  def switchTests   = List(switch_m2p_m, switch_m2p_p, switch_p2m_m, switch_p2m_p)
  def switchVcTests = List(switch_vc_m2p_m, switch_vc_m2p_p, switch_vc_p2m_m, switch_vc_p2m_p)

  implicit class NameOps[N <: Name](private val name: N) extends AnyVal {
    def chSuff(ch: Char) = name match {
      case n @ Term.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
      case n @ Type.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
    }
  }

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = param.copy(mods = param.mods :+ Mod.ValParam())
  }

  sealed trait ClassVariant
  object ClassVariant {
    case object Case     extends ClassVariant
    case object Value    extends ClassVariant
    case object Runnable extends ClassVariant
  }

  sealed trait ClassMethOverride
  object ClassMethOverride {
    case object No        extends ClassMethOverride
    case object  JavaMeth extends ClassMethOverride
    case object ScalaMeth extends ClassMethOverride
  }

  final case class Cls(variants: List[ClassVariant]) {
    val name: Type.Name = variants.foldRight(t"CR") {
      case (ClassVariant.Case,     name) => name.copy("C" + name.value)
      case (ClassVariant.Value,    name) => name.copy("V" + name.value)
      case (ClassVariant.Runnable, name) => name
    }

    val defn = {
      if (variants.isEmpty) q"class $name".withRunnable
      else {
        variants.foldLeft(q"class $name") {
          case (cls, ClassVariant.Case)     => cls.toCaseClass
          case (cls, ClassVariant.Value)    => cls.toValueClass
          case (cls, ClassVariant.Runnable) => cls.withRunnable
        }
      }
    }

    val defns = List(
      defn,
      defn.copy(name = defn.name.chSuff('S')).addStat(q"""override def toString   = """""),
      defn.copy(name = defn.name.chSuff('J')).addStat(q"""override def toString() = """""),
    )
  }

  val any  = Val(q"any", t"Any")
  val ref  = Val(q"ref", t"AnyRef")
  val obj  = Val(q"obj", t"Object")
  val str  = Val(q"str", t"String")

  val   CR = Cls(List(ClassVariant.Runnable))
  val  CCR = Cls(List(ClassVariant.Runnable, ClassVariant.Case))
  val  VCR = Cls(List(ClassVariant.Value))
  val VCCR = Cls(List(ClassVariant.Value, ClassVariant.Case))

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addStat(stat: Stat) = cls.copy(templ = cls.templ.copy(stats = cls.templ.stats :+ stat))
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = init :: cls.templ.inits))

    def toCaseClass = cls.copy(
      mods = cls.mods :+ Mod.Case(),
      ctor = cls.ctor.copy(paramss = (if (cls.ctor.paramss.isEmpty) List(Nil) else cls.ctor.paramss).map(_.map(_.notValParam))),
    )

    def toValueClass = cls.addInit(init"AnyVal").copy(
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil           => List(List(param"val x: String"))
        case List(List(p)) => List(List(p.toValParam))
        case paramss       => sys.error(s"Can't toValueClass ${cls.name} b/c of paramss: $paramss")
      }),
    )

    def withRunnable = addInit(init"Runnable").addStat(q"def run() = ()")
  }

  final case class Val(name: Term.Name, tpe: Type.Name) {
    val defn = Defn.Val(Nil, List(Pat.Var(name)), Option(tpe), Lit.String(""))
  }

  def duo(qual: Term, name: Term.Name) = List(List(q"$qual.$name", q"$qual.$name()"))

  def multi(msg2: Msg, msg3: Msg) =
    List(List(msg2), List(msg2), List(msg2), List(msg3), List(msg3), List(msg3), List(msg3))

  def multi2(msg2W: List[Msg], msg2E: List[Msg], msg3W: List[Msg], msg3E: List[Msg]) =
    List(msg2W, msg2W, msg2E, msg3W, msg3E, msg3E, msg3E)

  def multi3(msgs: (SV, Severity) => List[Msg]) =
    List(msgs(S2, Warn), msgs(S2, Warn), msgs(S2, Error), msgs(S3, Warn), msgs(S3, Error), msgs(S3, Error), msgs(S3, Error))

  val M  = q"trait M              { def d() : String }"
  val P  = q"trait P              { def d   : String }"
  val MU = q"trait MU extends Any { def d() : String }"
  val PU = q"trait PU extends Any { def d   : String }"

  val M2P    = q"""class M2P                   extends             M  { def d   = "" }"""
  val P2M    = q"""class P2M                   extends             P  { def d() = "" }"""
  val M2P_VC = q"""class M2P_VC(val x: String) extends AnyVal with MU { def d   = "" }"""
  val P2M_VC = q"""class P2M_VC(val x: String) extends AnyVal with PU { def d() = "" }"""

  val m2p    = q"""val m2p    = new M2P"""
  val p2m    = q"""val p2m    = new P2M"""
  val m2p_vc = q"""val m2p_vc = new M2P_VC("")"""
  val p2m_vc = q"""val p2m_vc = new P2M_VC("")"""

  import ErrorMsgs._

  sealed trait MethPropSwitch
  case object Meth2Prop extends MethPropSwitch
  case object Prop2Meth extends MethPropSwitch

  sealed trait SV; case object S2 extends SV; case object S3 extends SV

  def overrideM(sv: SV, switch: MethPropSwitch, sev: Severity, traitName: String) = (sv, switch) match {
    case (S2, Meth2Prop) => msg(sev, 3, override2_meth2prop)
    case (S3, Meth2Prop) => msg(sev, 3, override3_meth2prop(sev, traitName))
    case (S2, Prop2Meth) => msg(sev, 3, override2_prop2meth(sev, traitName))
    case (S3, Prop2Meth) => msg(sev, 3, override3_prop2meth(sev, traitName))
  }

  def autoApp(sv: SV, meth: String) = sv match { case S2 => autoApp2(meth) case S3 => autoApp3(meth) }

  def m2p_m_msgs(traitName: String) = multi3 {
    case (sv, sev) => List(overrideM(sv, Meth2Prop, sev, traitName))
  }

  def m2p_p_msgs(traitName: String) = multi3 {
    case (sv @ S2, sev) => List(overrideM(sv, Meth2Prop, sev, traitName), warn(5, autoApp(sv, "d")))
    case (sv @ S3, sev) => List(overrideM(sv, Meth2Prop, sev, traitName))
  }

  def p2m_m_msgs(traitName: String) = multi3 {
    case (sv, sev) => List(overrideM(sv, Prop2Meth, sev, traitName))
  }

  def p2m_p_msgs(traitName: String) = multi3 {
    case (S2, sev)         => List(overrideM(S2, Prop2Meth, sev, traitName), msg(Warn, 5, autoApp(S2, "d")))
    case (S3, sev @ Warn)  => List(overrideM(S3, Prop2Meth, sev, traitName), msg(sev,  5, autoApp(S3, "d")))
    case (S3, sev @ Error) => List(                                          msg(sev,  5, autoApp(S3, "d")))
    case ( _, Info)        => Nil
  }

  sealed class SwitchFile(
      val name: String, traitDefn: Defn.Trait, clsDefn: Defn.Class, valDefn: Defn.Val, stat: Stat,
      msgs: String => List[List[Msg]]
  ) extends MkInMemoryTestFile {
    val contents = TestContents(List(traitDefn, clsDefn, valDefn), List(List(stat)), msgs(traitDefn.name.value))
  }

  object switch_m2p_m    extends SwitchFile("Call.switch/m2p_m",    M,  M2P,    m2p,    q"m2p.d()",    m2p_m_msgs)
  object switch_m2p_p    extends SwitchFile("Call.switch/m2p_p",    M,  M2P,    m2p,    q"m2p.d",      m2p_p_msgs)
  object switch_p2m_m    extends SwitchFile("Call.switch/p2m_m",    P,  P2M,    p2m,    q"p2m.d()",    p2m_m_msgs)
  object switch_p2m_p    extends SwitchFile("Call.switch/p2m_p",    P,  P2M,    p2m,    q"p2m.d",      p2m_p_msgs)
  object switch_vc_m2p_m extends SwitchFile("Call.switch_vc/m2p_m", MU, M2P_VC, m2p_vc, q"m2p_vc.d()", m2p_m_msgs)
  object switch_vc_m2p_p extends SwitchFile("Call.switch_vc/m2p_p", MU, M2P_VC, m2p_vc, q"m2p_vc.d",   m2p_p_msgs)
  object switch_vc_p2m_m extends SwitchFile("Call.switch_vc/p2m_m", PU, P2M_VC, p2m_vc, q"p2m_vc.d()", p2m_m_msgs)
  object switch_vc_p2m_p extends SwitchFile("Call.switch_vc/p2m_p", PU, P2M_VC, p2m_vc, q"p2m_vc.d",   p2m_p_msgs)

  object def_meth_p extends MkInMemoryTestFile {
    val name     = "Call.meth_p"
    val msgs2    = List(warn(3, autoApp2("meth")))
    def msgs3(sev: Severity) = List(msg(sev, 3, autoApp3("meth")))
    val msgs     = List(msgs2, msgs2, msgs2, msgs3(Warn), msgs3(Error), msgs3(Error), msgs3(Error))
    val contents = TestContents(List(q"""def meth() = """""), List(List(q"meth")), msgs)
  }

  object def_prop_m extends MkInMemoryTestFile {
    val name     = "Call.prop_m"
    val err2     = err(3, "not enough arguments for method apply: (i: Int): Char in class StringOps.\nUnspecified value parameter i.")
    val err3     = err(3, "missing argument for parameter i of method apply: (i: Int): Char")
    val contents = TestContents(List(q"""def prop = """""), List(List(q"prop()")), multi(err2, err3))
  }

  object hashHash extends MkInMemoryTestFile {
    val name              = "Call.##"
    def err2(lineNo: Int) = err(lineNo, "Int does not take parameters")
    def err3(lineNo: Int) = err(lineNo, "method ## in class Any does not take parameters")
    val contents          = List(
      TestContents(List(any.defn), duo(any.name, q"##"), multi(err2( 7), err3( 7))),
      TestContents(List(ref.defn), duo(ref.name, q"##"), multi(err2( 9), err3( 9))),
      TestContents(List(obj.defn), duo(obj.name, q"##"), multi(err2(11), err3(11))),
      TestContents(List(str.defn), duo(str.name, q"##"), multi(err2(13), err3(13))),
    ).reduce(_ ++ _).toUnit
  }

  object pos extends MkInMemoryTestFile {
    val name  = "Call.pos"
    val vals  = List(any, ref, obj, str)
    val defns = CR.defns ::: CCR.defns ::: VCR.defns ::: VCCR.defns ::: vals.map(_.defn)

    def alt(t: Term, suff: Char) = t match {
      case q"new ${n @ Type.Name(_)}(...$argss)" => q"new ${n.chSuff(suff)}(...$argss)"
      case q"${n @ Term.Name(_)}(..$args)"       => q"${n.chSuff(suff)}(..$args)"
    }

    def toStrings(r: Term)       = duo(r, q"toString") ::: duo(alt(r, 'S'), q"toString") ::: duo(alt(r, 'J'), q"toString")
    def toStringsAndRun(r: Term) = duo(r, q"run") ::: toStrings(r)

    val stats = vals.map(_.name).flatMap { nme =>
        duo(nme, q"getClass") ::: duo(nme, q"hashCode") ::: duo(nme, q"toString")
      } :::
        toStringsAndRun(q"new CR()")  :::
        toStrings(q"""new VCR("")""") :::
        toStringsAndRun(q"CCR()")     :::
        toStrings(q"""VCCR("")""")

    def contents = TestContents(defns, stats, List(Nil, Nil, Nil, Nil, Nil, Nil, Nil))
  }
}
