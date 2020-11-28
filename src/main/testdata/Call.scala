package shisa
package testdata

import scala.meta._

object Call {
  def tests: List[TestFile] = List(hashHash, pos).map(_.testFile)

  sealed trait ClsOpt
  case object CaseCls extends ClsOpt; case object ValCls extends ClsOpt; case object RunCls extends ClsOpt

  final case class Cls(variants: List[ClsOpt], suffix: String = "R") {
    val name: Type.Name = variants.foldRight(Type.Name(s"C$suffix")) {
      case (CaseCls, name) => name.copy("C" + name.value)
      case ( ValCls, name) => name.copy("V" + name.value)
      case ( RunCls, name) => name
    }
    val termName: Term.Name = Term.Name(name.value)

    val defn: Defn.Class = variants.foldLeft(q"class $name") {
      case (cls, CaseCls) => cls.toCaseClass
      case (cls,  ValCls) => cls.toValueClass
      case (cls,  RunCls) => cls.withRunnable
    }

    val inst: Term = {
      val args = if (defn.isValueClass) List(Lit.String("")) else Nil
      if (defn.isCaseClass) q"$termName(..$args)" else q"new $name(..$args)"
    }

    lazy val copyS = copy(suffix = "S")
    lazy val copyJ = copy(suffix = "J")
    lazy val defnS = copyS.defn.addStat(q"override def toString   = $ns")
    lazy val defnJ = copyJ.defn.addStat(q"override def toString() = $ns")
    lazy val defns = List(defn, defnS, defnJ)
  }

  val any  = Val(q"any", t"Any")
  val ref  = Val(q"ref", t"AnyRef")
  val obj  = Val(q"obj", t"Object")
  val str  = Val(q"str", t"String")
  val vals = List(any, ref, obj, str)
  val   CR = Cls(List(RunCls))
  val  CCR = Cls(List(RunCls, CaseCls))
  val  VCR = Cls(List(ValCls))
  val VCCR = Cls(List(ValCls, CaseCls))
  val cls1 = List(CR, CCR, VCR, VCCR)
  val clss = cls1.flatMap(cls => List(cls, cls.copyS, cls.copyJ))

  final case class Val(name: Term.Name, tpe: Type.Name) {
    val defn = Defn.Val(Nil, List(Pat.Var(name)), Option(tpe), Lit.String(""))
  }

  object hashHash extends MkInMemoryTestFile {
    val name     = "Call.##"
    val err2     = err(                   "Int does not take parameters")
    val err3     = err("method ## in class Any does not take parameters")
    val mkCnts   = (v: Val) => TestContents(List(v.defn), List(List(q"${v.name}.##()")), multi(err2, err3))
    val contents = vals.map(mkCnts).reduce(_ ++ _).toUnit
  }

  object pos extends MkInMemoryTestFile {
    val name  = "Call.pos"
    val defns = cls1.flatMap(_.defns) ::: vals.map(_.defn)
    val stats =
      vals.map          { v   => List(q"${v.name}.##")                                    } :::
      vals.map          { v   => List(q"${v.name}.toString",   q"${v.name}.toString()")   } :::
      vals.map          { v   => List(q"${v.name}.getClass",   q"${v.name}.getClass()")   } :::
      vals.map          { v   => List(q"${v.name}.hashCode",   q"${v.name}.hashCode()")   } :::
      clss.map          { cls => List(q"${cls.inst}.toString", q"${cls.inst}.toString()") } :::
      List(CR, CCR).map { cls => List(q"${cls.inst}.run",      q"${cls.inst}.run()")      }
    def contents = TestContents(defns, stats, noMsgs).toUnit
  }
}
