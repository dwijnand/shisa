package shisa

import scala.meta._

object Call {
  def tests: List[TestFile] = List(hashHash, pos)

  // Types
  sealed trait ClsOpt
  case object CaseCls extends ClsOpt; case object ValCls extends ClsOpt; case object RunCls extends ClsOpt

  def nul(qual: Term, name: Term.Name) = q"$qual.$name"
  def nil(qual: Term, name: Term.Name) = q"$qual.$name()"
  def two(qual: Term, name: Term.Name) = List(nul(qual, name), nil(qual, name))

  final case class Val(name: Term.Name, tpe: Type.Name) {
    val defn = Defn.Val(Nil, List(Pat.Var(name)), Option(tpe), Lit.String(""))
  }

  final case class Cls(variants: List[ClsOpt], suffix: String = "R") {
    val name: Type.Name = variants.foldRight(Type.Name(s"C$suffix")) {
      case (CaseCls, name) => name.copy("C" + name.value)
      case ( ValCls, name) => name.copy("V" + name.value)
      case ( RunCls, name) => name
    }
    val defn: Defn.Class = variants.foldLeft(q"class $name") {
      case (cls, CaseCls) => cls.toCaseClass
      case (cls,  ValCls) => cls.toValueClass
      case (cls,  RunCls) => cls.withRunnable
    }

    val inst: Term = {
      val args = if (defn.isValueClass) List(Lit.String("")) else Nil
      if (defn.isCaseClass) q"${name.toTermName}(..$args)" else q"new $name(..$args)"
    }

    lazy val copyS = copy(suffix = "S")
    lazy val copyJ = copy(suffix = "J")
    lazy val defnS = copyS.defn.addStat(q"override def ${nme.toString_}   = $ns")
    lazy val defnJ = copyJ.defn.addStat(q"override def ${nme.toString_}() = $ns")
    lazy val defns = List(defn, defnS, defnJ)
  }

  val any  = Val(q"any", tpnme.Any)
  val ref  = Val(q"ref", tpnme.AnyRef)
  val obj  = Val(q"obj", tpnme.Object)
  val str  = Val(q"str", tpnme.String)
  val   CR = Cls(List(RunCls))
  val  CCR = Cls(List(RunCls, CaseCls))
  val  VCR = Cls(List(ValCls))
  val VCCR = Cls(List(ValCls, CaseCls))

  val vals = List(any, ref, obj, str)
  val clsR = List(CR, CCR)
  val clss = List(CR, CCR, VCR, VCCR)
  val clsU = clss.flatMap(cls => List(cls, cls.copyS, cls.copyJ))

  val hashHashErr2 = err(                   "Int does not take parameters")
  val hashHashErr3 = err("method ## in class Any does not take parameters")
  val hashHashErrs = multi(hashHashErr2, hashHashErr3)

  val hashHash = {
    val mkStat = (v: Val) => nil(v.name, nme.hashHash)
    val mkOne  = (v: Val) => TestContents(List(v.defn), List(List(mkStat(v))), hashHashErrs)
    val tests  = vals.map(mkOne).reduce(_ ++ _)
    TestFile("Call.##", TestContents(tests.defns, List(tests.stats.flatten), tests.msgs))
  }

  val pos = {
    val defns = clss.flatMap(_.defns) ::: vals.map(_.defn)
    val stats =
      vals.map { v => List(nul(v.name, nme.hashHash)) } :::
      vals.map { v => two(v.name, nme.toString_)      } :::
      vals.map { v => two(v.name, nme.getClass_)      } :::
      vals.map { v => two(v.name, nme.hashCode_)      } :::
      clsU.map { c => two(c.inst, nme.toString_)      } :::
      clsR.map { c => two(c.inst, nme.run)            } :::
      Nil
    val mkOne  = (v: Val) => TestContents(List(v.defn), Nil, noMsgs)
    val tests  = vals.map(mkOne).reduce(_ ++ _)
    TestFile("Call.pos", TestContents(defns, List(stats.flatten), tests.msgs))
  }

  // Names
  object nme {
    val getClass_ = q"getClass"
    val hashCode_ = q"hashCode"
    val hashHash  = q"##"
    val run       = q"run"
    val toString_ = q"toString"
  }

  object tpnme {
    val Any    = t"Any"
    val AnyRef = t"AnyRef"
    val Object = t"Object"
    val String = t"String"
  }
}
