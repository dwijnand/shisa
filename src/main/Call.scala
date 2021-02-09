package shisa

import scala.meta._, contrib._

object Call {
  def tests: List[TestFile] = List(hashHash, pos)

  // Types
  sealed trait ClsOpt
  case object CaseCls extends ClsOpt; case object ValCls extends ClsOpt; case object RunCls extends ClsOpt

  def nul(qual: Term, name: Term.Name) = Term.Select(qual, name)
  def nil(qual: Term, name: Term.Name) = Term.Apply(Term.Select(qual, name), Nil)
  def two(qual: Term, name: Term.Name) = List(nul(qual, name), nil(qual, name))

  final case class Val(name: Term.Name, tpe: Type.Name) {
    val defn = Defn.Val(Nil, List(Pat.Var(name)), Option(tpe), Lit.String(""))
  }

  final case class Cls(variants: List[ClsOpt], suffix: String = "R") {
    val defn: Defn.Class = variants.foldLeft(q"class ${Type.Name(s"C$suffix")}") {
      case (cls, CaseCls) => cls.copy(name = cls.name.copy("C" + cls.name.value)).toCaseClass
      case (cls,  ValCls) => cls.copy(name = cls.name.copy("V" + cls.name.value)).toValueClass
      case (cls,  RunCls) => cls.withRunnable
    }
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
  val clsR = List(CR, CCR).map(_.defn)
  val clss = List(CR, CCR, VCR, VCCR)
  val clsV = clss.flatMap { cls =>
    val defnS = cls.copy(suffix = "S").defn.addStat(q"override def ${nme.toString_}   = $ns")
    val defnJ = cls.copy(suffix = "J").defn.addStat(q"override def ${nme.toString_}() = $ns")
    List(cls.defn, defnS, defnJ)
  }

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
    val defns = clsV ::: vals.map(_.defn)
    val stats =
      vals.map     { v => nul(v.name, nme.hashHash)  } :::
      vals.flatMap { v => two(v.name, nme.toString_) } :::
      vals.flatMap { v => two(v.name, nme.getClass_) } :::
      vals.flatMap { v => two(v.name, nme.hashCode_) } :::
      clsV.flatMap { c => two(c.inst, nme.toString_) } :::
      clsR.flatMap { c => two(c.inst, nme.run)       } :::
      Nil
    val mkOne  = (v: Val) => TestContents(List(v.defn), Nil, noMsgs)
    val tests  = vals.map(mkOne).reduce(_ ++ _)
    TestFile("Call.pos", TestContents(defns, List(stats), tests.msgs))
  }
}
