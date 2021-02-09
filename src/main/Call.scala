package shisa

import scala.meta._, contrib._

object Call {
  def tests: List[TestFile] = List(neg, pos)

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

  val negTests  = for (v <- vals; stat  = nil(v.name, nme.hashHash )) yield mk(v.defn, stat, hashHashErrs)
  val posTests1 = for (v <- vals; stat  = nul(v.name, nme.hashHash )) yield mk(v.defn, stat, noMsgs)
  val posTests2 = for (v <- vals; stat <- two(v.name, nme.toString_)) yield mk(v.defn, stat, noMsgs)
  val posTests3 = for (v <- vals; stat <- two(v.name, nme.getClass_)) yield mk(v.defn, stat, noMsgs)
  val posTests4 = for (v <- vals; stat <- two(v.name, nme.hashCode_)) yield mk(v.defn, stat, noMsgs)
  val posTests5 = for (c <- clsV; stat <- two(c.inst, nme.toString_)) yield mk(     c, stat, noMsgs)
  val posTests6 = for (c <- clsR; stat <- two(c.inst, nme.run      )) yield mk(     c, stat, noMsgs)
  val posTests  = posTests1 ::: posTests2 ::: posTests3 ::: posTests4 ::: posTests5 ::: posTests6

  val neg = mkFile("Call.neg", negTests)
  val pos = mkFile("Call.pos", posTests)

  def mk(defn: Defn, stat: Stat, msgs: List[List[Msg]]) = TestContents(List(defn), List(List(stat)), msgs)
  def mkFile(name: String, ts: List[TestContents])      = TestFile(name, Test.toContents(ts).toUnit)
}
