package shisa

package object testdata {
  val Warn  = Severity.Warn
  val Error = Severity.Error

  val ns = "" // empty string ("no string")

  val noMsgs = multi3((_, _) => Nil)

  def multi(msg2: Msg, msg3: Msg) =
    List(List(msg2), List(msg2), List(msg3), List(msg3), List(msg3), List(msg3))

  def multi3(msgs: (SV, WorE) => List[Msg]) =
    List(msgs(S2, W), msgs(S2, E), msgs(S3, W), msgs(S3, E), msgs(S3, E), msgs(S3, E))

  def multi4(msgs2: WorE => List[Msg], msgs30: WorE => List[Msg], msgs31: WorE => List[Msg]) =
    List(msgs2(W), msgs2(E), msgs30(W), msgs30(E), msgs31(W), msgs31(E))

  def multi5(msgs: (BSV, WorE) => List[Msg]) =
    List(msgs(S2X, W), msgs(S2X, E), msgs(S30, W), msgs(S30, E), msgs(S31, W), msgs(S31, E))

  def  msg(sev: Severity, str: String) = new Msg(sev, str)
  def warn(str: String)                = msg(Warn,  str)
  def  err(str: String)                = msg(Error, str)

  def anyWarn = warn("*")
  def anyErr  =  err("*")

  def autoApp(sv: SV, meth: String) = sv match { case S2 => autoApp2(meth) case S3 => autoApp3(meth) }
  def autoApp2(meth: String) =
    s"""Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method $meth,
       |or remove the empty argument list from its definition (Java-defined methods are exempt).
       |In Scala 3, an unapplied method like this will be eta-expanded into a function.""".stripMargin
  def autoApp3(meth: String) = s"method $meth must be called with () argument"
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

