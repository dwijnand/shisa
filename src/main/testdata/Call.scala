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
  import ErrorMsgs._, Types._

  def tests = List(hashHash, pos, def_meth_p, def_prop_m).map(_.testFile)

  implicit class NameOps[N <: Name](private val name: N) extends AnyVal {
    def chSuff(ch: Char) = name match {
      case n @ Term.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
      case n @ Type.Name(v) => n.copy(value = v.dropRight(1) + ch).asInstanceOf[N]
    }
  }

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = if (param.mods.exists(_.is[Mod.ValParam])) param else param.copy(mods = param.mods :+ Mod.ValParam())
  }

  sealed trait ClassVariant
  object ClassVariant { case object Case extends ClassVariant; case object Value extends ClassVariant; case object Runnable extends ClassVariant }

  sealed trait Override
  object Override { case object No extends Override; case object Java extends Override; case object Scala extends Override }

  final case class Cls(variants: List[ClassVariant]) {
    val name: Type.Name = variants.foldRight(t"CR") {
      case (ClassVariant.Case,     name) => name.copy("C" + name.value)
      case (ClassVariant.Value,    name) => name.copy("V" + name.value)
      case (ClassVariant.Runnable, name) => name
    }

    val defn: Defn.Class = {
      if (variants.isEmpty) q"class $name".withRunnable
      else {
        variants.foldLeft(q"class $name") {
          case (cls, ClassVariant.Case)     => cls.toCaseClass
          case (cls, ClassVariant.Value)    => cls.toValueClass
          case (cls, ClassVariant.Runnable) => cls.withRunnable
        }
      }
    }

    val defnS = defn.copy(name = defn.name.chSuff('S')).addStat(q"""override def toString   = """"")
    val defnJ = defn.copy(name = defn.name.chSuff('J')).addStat(q"""override def toString() = """"")
    val defns = List(defn, defnS, defnJ)
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
    multi2(List(msg2), List(msg2), List(msg3), List(msg3))

  def multi2(msg2W: List[Msg], msg2E: List[Msg], msg3W: List[Msg], msg3E: List[Msg]) =
    List(msg2W, msg2E, msg3W, msg3E, msg3E, msg3E)

  def multi3(msgs: (SV, WorE) => List[Msg]) = multi2(msgs(S2, W), msgs(S2, E), msgs(S3, W), msgs(S3, E))

  object def_meth_p extends MkInMemoryTestFile {
    val name     = "Call.meth_p"
    val msgs     = multi3 {
      case (S2, _) => List(msg(Warn,  3, autoApp2("meth")))
      case (S3, W) => List(msg(Warn,  3, autoApp3("meth")))
      case (S3, E) => List(msg(Error, 3, autoApp3("meth")))
    }
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
