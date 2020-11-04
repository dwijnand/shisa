package shisa
package testdata

import java.nio.file._

import scala.meta._

import Severity.{ Warn, Error }

object MkInMemoryTestFile {
  def  msg(sev: Severity, path: Path, lineNo: Int, str: String) = new Msg(sev,  s"target/testdata/$path", lineNo, str, "")
  def warn(path: Path, lineNo: Int, str: String)                = msg(Severity.Warn,  path, lineNo, str)
  def  err(path: Path, lineNo: Int, str: String)                = msg(Severity.Error, path, lineNo, str)
}

trait MkInMemoryTestFile {
  def path: Path
  def contents: TestContents
}

trait MkInMemoryTestUnitFile extends MkInMemoryTestFile {
  final def warn(lineNo: Int, str: String) = MkInMemoryTestFile.warn(path, lineNo, str)
  final def  err(lineNo: Int, str: String) = MkInMemoryTestFile.err( path, lineNo, str)
}

object Call {
  def idF[A]: A => A = x => x

  def tests         = List(hashHash, pos, def_meth_p, def_prop_m) ::: switchTests ::: switchVcTests
  def switchTests   = List(switch_m2p_m, switch_m2p_p, switch_p2m_m, switch_p2m_p)
  def switchVcTests = List(switch_vc_m2p_m, switch_vc_m2p_p, switch_vc_p2m_m, switch_vc_p2m_p)

  implicit class ListOps[A](private val xs: List[A]) extends AnyVal {
    def onNil[B](z: => B, f: List[A] => B): B = if (xs.isEmpty) z else f(xs)
  }

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
  val vals = List(any, ref, obj, str)

  val   CR = Cls(List(ClassVariant.Runnable))
  val  CCR = Cls(List(ClassVariant.Runnable, ClassVariant.Case))
  val  VCR = Cls(List(ClassVariant.Value))
  val VCCR = Cls(List(ClassVariant.Value, ClassVariant.Case))

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addStat(stat: Stat) = cls.copy(templ = cls.templ.copy(stats = cls.templ.stats :+ stat))
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = init :: cls.templ.inits))

    def toCaseClass = cls.copy(
      mods = cls.mods :+ Mod.Case(),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss.onNil(List(Nil), idF).map(_.map(_.notValParam))),
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

  def duo(qual: Term, name: Term.Name) = List(q"$qual.$name", q"$qual.$name()")

  def multis(msgs2: List[Msg], msgs3: List[Msg]) = List(msgs2, msgs2, msgs2,
                                                        msgs3, msgs3, msgs3, msgs3)
  def multi(msg2: Msg, msg3: Msg)                = multis(List(msg2), List(msg3))
  val noMsgs                                     = multis(Nil, Nil)

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
  import MkInMemoryTestFile.{ err, warn }

  def m2p_m_msgs(path: Path, traitName: String) = {
    def warns2 = List(warn(path, 3, errOverride2))
    def  errs2 = List( err(path, 3, errOverride2))
    def warns3 = List(warn(path, 3, errOverride3A(traitName, str1, str2)))
    def  errs3 = List( err(path, 3, errOverride3B(traitName, str1, str2)))
    List(warns2, warns2, errs2, warns3, errs3, errs3, errs3)
  }

  def m2p_p_msgs(path: Path, traitName: String) = {
    def warns2   = List(warn(path, 5, autoApp2("d")), warn(path, 3, errOverride2))
    def warnErr2 = List( err(path, 3, errOverride2),  warn(path, 5, autoApp2("d")))
    def warns3   = List(warn(path, 3, errOverride3A(traitName, str1, str2)))
    def  errs3   = List( err(path, 3, errOverride3B(traitName, str1, str2)))
    List(warns2, warns2, warnErr2, warns3, errs3, errs3, errs3)
  }

  def p2m_m_msgs(path: Path, traitName: String) = {
    def warns2 = List(warn(path, 3, p2mMsg))
    def errs2  = List( err(path, 3, p2mErr(traitName)))
    def warns3 = List(warn(path, 3, errOverride3A(traitName, str2, str1)))
    def  errs3 = List( err(path, 3, errOverride3B(traitName, str2, str1)))
    List(warns2, warns2, errs2, warns3, errs3, errs3, errs3)
  }

  def p2m_p_msgs(path: Path, traitName: String) = {
    def warns2   = List(warn(path, 5, autoApp2("d")),    warn(path, 3, p2mMsg))
    def warnErr2 = List(warn(path, 5, autoApp2("d")),     err(path, 3, p2mErr(traitName)))
    def warns3   = List(warn(path, 5, parensCall3("d")), warn(path, 3, errOverride3A(traitName, str2, str1)))
    def  errs3   = List( err(path, 5, parensCall3("d")))
    List(warns2, warns2, warnErr2, warns3, errs3, errs3, errs3)
  }

  sealed class SwitchFile(
      pathStr: String, traitDefn: Defn.Trait, clsDefn: Defn.Class, valDefn: Defn.Val, stat: Stat,
      msgs: (Path, String) => List[List[Msg]]
  ) extends MkInMemoryTestUnitFile {
    val path     = Paths.get(pathStr)
    val contents = TestContents(List(traitDefn, clsDefn, valDefn), List(stat), msgs(path, traitDefn.name.value))
  }

  object switch_m2p_m    extends SwitchFile("Call.switch/Call.m2p_m.scala", M,  M2P,    m2p,    q"m2p.d()",    m2p_m_msgs)
  object switch_m2p_p    extends SwitchFile("Call.switch/Call.m2p_p.scala", M,  M2P,    m2p,    q"m2p.d",      m2p_p_msgs)
  object switch_p2m_m    extends SwitchFile("Call.switch/Call.p2m_m.scala", P,  P2M,    p2m,    q"p2m.d()",    p2m_m_msgs)
  object switch_p2m_p    extends SwitchFile("Call.switch/Call.p2m_p.scala", P,  P2M,    p2m,    q"p2m.d",      p2m_p_msgs)
  object switch_vc_m2p_m extends SwitchFile("Call.switch_vc/m2p_m.scala",   MU, M2P_VC, m2p_vc, q"m2p_vc.d()", m2p_m_msgs)
  object switch_vc_m2p_p extends SwitchFile("Call.switch_vc/m2p_p.scala",   MU, M2P_VC, m2p_vc, q"m2p_vc.d",   m2p_p_msgs)
  object switch_vc_p2m_m extends SwitchFile("Call.switch_vc/p2m_m.scala",   PU, P2M_VC, p2m_vc, q"p2m_vc.d()", p2m_m_msgs)
  object switch_vc_p2m_p extends SwitchFile("Call.switch_vc/p2m_p.scala",   PU, P2M_VC, p2m_vc, q"p2m_vc.d",   p2m_p_msgs)

  object def_meth_p extends MkInMemoryTestUnitFile {
    val path     = Paths.get("Call.def/Call.meth_p.scala")
    val warns2   = List(warn(3, autoApp2("meth")))
    val msgs3Old = List(warn(3, parensCall3("meth")))
    val msgs3    = List( err(3, parensCall3("meth")))
    val msgs     = List(warns2, warns2, warns2, msgs3Old, msgs3, msgs3, msgs3)
    val contents = TestContents(List(q"""def meth() = """""), List(q"meth"), msgs)
  }

  object def_prop_m extends MkInMemoryTestUnitFile {
    val path     = Paths.get("Call.def/Call.prop_m.scala")
    val err2     = err(3, "not enough arguments for method apply: (i: Int): Char in class StringOps.\nUnspecified value parameter i.")
    val err3     = err(3, "missing argument for parameter i of method apply: (i: Int): Char")
    val contents = TestContents(List(q"""def prop = """""), List(q"prop()"), multi(err2, err3))
  }

  object hashHash extends MkInMemoryTestUnitFile {
    val path     = Paths.get("Call.##.scala")
    val contents = List(
      TestContents(List(any.defn), duo(any.name, q"##"), multi(err2( 7), err3( 7))),
      TestContents(List(ref.defn), duo(ref.name, q"##"), multi(err2( 9), err3( 9))),
      TestContents(List(obj.defn), duo(obj.name, q"##"), multi(err2(11), err3(11))),
      TestContents(List(str.defn), duo(str.name, q"##"), multi(err2(13), err3(13))),
    ).reduce(_ ++ _)
    def err2(lineNo: Int) = err(lineNo, "Int does not take parameters")
    def err3(lineNo: Int) = err(lineNo, "method ## in class Any does not take parameters")
  }

  object pos extends MkInMemoryTestUnitFile {
    val path  = Paths.get("Call.pos.scala")
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

    def contents = TestContents(defns, stats, noMsgs)
  }
}
