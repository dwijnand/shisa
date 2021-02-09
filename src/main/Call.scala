package shisa

import scala.meta._

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
    val name: Type.Name = variants.foldRight(Type.Name(s"C$suffix")) {
      case (CaseCls, name) => name.copy("C" + name.value)
      case ( ValCls, name) => name.copy("V" + name.value)
      case ( RunCls, name) => name
    }
    val baseTmpl = Template(Nil, Nil, Self(Name(""), None), Nil)
    val baseCls  = Defn.Class(Nil, name, Nil, Ctor.Primary(Nil, Name(""), Nil), baseTmpl)
    val defn: Defn.Class = variants.foldLeft(baseCls) {
      case (cls, CaseCls) => cls.toCaseClass
      case (cls,  ValCls) => cls.toValueClass
      case (cls,  RunCls) => cls.withRunnable
    }

    val inst: Term = variants match {
      case List(ValCls, CaseCls) => Term.Apply(defn.name.toTermName, List(Lit.String("")))
      case List(RunCls, CaseCls) => Term.Apply(defn.name.toTermName, Nil)
      case List(RunCls)          => Term.New(Init(defn.name, Name(""), List(Nil)))
      case List(ValCls)          => Term.New(Init(defn.name, Name(""), List(List(Lit.String("")))))
      case _                     => sys.error(s"Unexpected variants: $variants")
    }

    lazy val copyS = copy(suffix = "S")
    lazy val copyJ = copy(suffix = "J")
    lazy val defnS = copyS.defn.addStat(Defn.Def(List(Mod.Override()), nme.toString_, Nil,      Nil , None, Lit.String("")))
    lazy val defnJ = copyJ.defn.addStat(Defn.Def(List(Mod.Override()), nme.toString_, Nil, List(Nil), None, Lit.String("")))
    lazy val defns = List(defn, defnS, defnJ)
  }

  val any  = Val(Term.Name("any"), tpnme.Any)
  val ref  = Val(Term.Name("ref"), tpnme.AnyRef)
  val obj  = Val(Term.Name("obj"), tpnme.Object)
  val str  = Val(Term.Name("str"), tpnme.String)
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
    val getClass_ = Term.Name("getClass")
    val hashCode_ = Term.Name("hashCode")
    val hashHash  = Term.Name("##")
    val run       = Term.Name("run")
    val toString_ = Term.Name("toString")
  }

  object tpnme {
    val Any    = Type.Name("Any")
    val AnyRef = Type.Name("AnyRef")
    val Object = Type.Name("Object")
    val String = Type.Name("String")
  }
}
