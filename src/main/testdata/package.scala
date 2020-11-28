package shisa

import scala.meta._, classifiers.{ Classifiable, Classifier }

package object testdata {
  val Warn  = Severity.Warn
  val Error = Severity.Error

  val ns: String = "" // empty string ("no string")

  val noMsgs                           = multi3((_, _) => Nil)
  def noMsgs(sev: Severity)            = Nil
  def msgs(mkMsg: Severity => Msg)     = (sev: Severity) => List(mkMsg(sev))
  def  msg(sev: Severity, str: String) = new Msg(sev, str)
  def warn(str: String)                = msg(Warn,  str)
  def  err(str: String)                = msg(Error, str)
  def anyErr                           = err("*")

  def multi(msg2: Msg, msg3: Msg) =
    List(List(msg2), List(msg2), List(msg3), List(msg3), List(msg3), List(msg3))

  def multi3(msgs: (SV, WorE) => List[Msg]) =
    List(msgs(S2, W), msgs(S2, E), msgs(S3, W), msgs(S3, E), msgs(S3, E), msgs(S3, E))

  def multi4(msgs2: WorE => List[Msg], msgs30: WorE => List[Msg], msgs31: WorE => List[Msg]) =
    List(msgs2(W), msgs2(E), msgs30(W), msgs30(E), msgs31(W), msgs31(E))

  def multi5(msgs: (BSV, WorE) => List[Msg]) =
    List(msgs(S2X, W), msgs(S2X, E), msgs(S30, W), msgs(S30, E), msgs(S31, W), msgs(S31, E))

  def autoApp(sv: SV, meth: String) = sv match { case S2 => autoApp2(meth) case S3 => autoApp3(meth) }
  def autoApp2(meth: String) =
    s"""Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method $meth,
       |or remove the empty argument list from its definition (Java-defined methods are exempt).
       |In Scala 3, an unapplied method like this will be eta-expanded into a function.""".stripMargin
  def autoApp3(meth: String) = s"method $meth must be called with () argument"

  implicit class ListOps[T](private val xs: List[T]) extends AnyVal {
    def has[U]   (implicit classifier: Classifier[T, U]): Boolean = xs.exists( classifier(_))
    def hasNot[U](implicit classifier: Classifier[T, U]): Boolean = xs.forall(!classifier(_))

    def appendOnce[U](x: T)(implicit classifier: Classifier[T, U])  = if (xs.has[U]) xs else xs :+ x
    def prependOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else x :: xs
  }

  implicit class TermParamOps(private val param: Term.Param) extends AnyVal {
    def notValParam = param.copy(mods = param.mods.filter(_.isNot[Mod.ValParam]))
    def  toValParam = param.copy(mods = param.mods.appendOnce(Mod.ValParam()))
  }

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addStat(stat: Stat) = cls.copy(templ = cls.templ.copy(stats = cls.templ.stats :+ stat))
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = cls.templ.inits.prependOnce(init)))

    def toCaseClass = cls.copy(
      mods = cls.mods.appendOnce(Mod.Case()),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil     => List(Nil)
        case paramss => paramss.map(_.map(_.notValParam))
      }),
    )

    def toValueClass = cls.addInit(init"AnyVal").copy(
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil           => List(List(param"val x: String"))
        case List(List(p)) => List(List(p.toValParam))
        case paramss       => sys.error(s"Can't toValueClass ${cls.name} b/c of paramss: $paramss")
      }),
    )

    def withRunnable = addInit(init"Runnable").addStat(q"def run() = ()")

    def isCaseClass  = cls.mods.has[Mod.Case]
    def isValueClass = cls.templ.inits.exists { case Init(Type.Name("AnyVal"), Name(""), Nil) => true case _ => false }

    def  name: Type.Name = cls.name
    def tname: Term.Name = Term.Name(name.value)
  }
}

import testdata._

sealed trait SV;   case object S2  extends  SV; case object S3  extends  SV
sealed trait BSV;  case object S2X extends BSV; case object S30 extends BSV; case object S31 extends BSV
sealed trait WorE; case object W  extends WorE; case object E  extends WorE

object WorE {
  implicit class Ops(private val wore: WorE) extends AnyVal {
    def toSev: Severity = wore match { case W => Warn case E => Error }
  }
}

trait MkInMemoryTestFile {
  def name: String
  def contents: TestContents
  final def testFile: TestFile = TestFile(name, contents)
}

