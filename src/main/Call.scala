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

  val any  = q"val any: ${tpnme.Any}    = ${Lit.String("")}"
  val ref  = q"val ref: ${tpnme.AnyRef} = ${Lit.String("")}"
  val obj  = q"val obj: ${tpnme.Object} = ${Lit.String("")}"
  val str  = q"val str: ${tpnme.String} = ${Lit.String("")}"
  val   CR = q"class   CR".withRunnable
  val  CCR = q"class  CCR".withRunnable.toCaseClass
  val  VCR = q"class  VCR".toValueClass
  val VCCR = q"class VCCR".toValueClass.toValueClass

  val vals = List(any, ref, obj, str)
  val clsR = List(CR, CCR)
  val clsV = List(CR, CCR, VCR, VCCR).flatMap { cls =>
    val s = cls.copy(name = Type.Name(cls.name.value + "S")).addStat(q"override def ${nme.toString_}   = $ns")
    val j = cls.copy(name = Type.Name(cls.name.value + "J")).addStat(q"override def ${nme.toString_}() = $ns")
    List(cls, s, j)
  }

  val hashHashErr2 = err(                   "Int does not take parameters")
  val hashHashErr3 = err("method ## in class Any does not take parameters")
  val hashHashErrs = multi(hashHashErr2, hashHashErr3)

  val neg1 = for (x <- vals; stat  = nil(x.inst, nme.hashHash )) yield mk(x, stat, hashHashErrs)
  val pos1 = for (x <- vals; stat  = nul(x.inst, nme.hashHash )) yield mk(x, stat, noMsgs)
  val pos2 = for (x <- vals; stat <- two(x.inst, nme.toString_)) yield mk(x, stat, noMsgs)
  val pos3 = for (x <- vals; stat <- two(x.inst, nme.getClass_)) yield mk(x, stat, noMsgs)
  val pos4 = for (x <- vals; stat <- two(x.inst, nme.hashCode_)) yield mk(x, stat, noMsgs)
  val pos5 = for (x <- clsV; stat <- two(x.inst, nme.toString_)) yield mk(x, stat, noMsgs)
  val pos6 = for (x <- clsR; stat <- two(x.inst, nme.run      )) yield mk(x, stat, noMsgs)
  val (neg, pos) = toNegAndPos("Call", neg1 ::: pos1 ::: pos2 ::: pos3 ::: pos4 ::: pos5 ::: pos6)

  def mk(defn: Defn, stat: Stat, msgs: List[List[Msg]]) = TestContents(List(defn), List(List(stat)), msgs)
  def mkFile(name: String, ts: List[TestContents])      = TestFile(name, Test.toContents(ts).toUnit)

  def toNegAndPos(name: String, tests: List[TestContents]) = {
    val (neg, pos) = tests.partition(_.msgs == noMsgs)
    (mkFile(s"$name.neg", neg), mkFile(s"$name.pos", pos))
  }
}
