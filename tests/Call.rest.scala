class CR extends Runnable { def run() = () }
class CS { override def toString   = "" }
class CJ { override def toString() = "" }

class VCR(val x: String) extends AnyVal
class VCS(val x: String) extends AnyVal { override def toString   = "" }
class VCJ(val x: String) extends AnyVal { override def toString() = "" }

case class CCR() extends Runnable { def run() = () }
case class CCS() { override def toString   = "" }
case class CCJ() { override def toString() = "" }

case class VCCR(x: String) extends AnyVal
case class VCCS(x: String) extends AnyVal { override def toString   = "" }
case class VCCJ(x: String) extends AnyVal { override def toString() = "" }

class Test {
  val any: Any    = ""
  val ref: AnyRef = ""

  val cr = new CR
  val cs = new CS
  val cj = new CJ

  val vcr = new VCR("")
  val vcs = new VCS("")
  val vcj = new VCJ("")

  val ccr = CCR()
  val ccs = CCS()
  val ccj = CCJ()

  val vccr = VCCR("")
  val vccs = VCCS("")
  val vccj = VCCJ("")

  any.getClass
  any.getClass()
  any.hashCode
  any.hashCode()
  any.toString
  any.toString()

  ref.getClass
  ref.getClass()
  ref.hashCode
  ref.hashCode()
  ref.toString
  ref.toString()

  cr.run
  cr.run()
  cr.toString
  cr.toString()
  cs.toString
  cs.toString()
  cj.toString
  cj.toString()

  vcr.toString
  vcr.toString()
  vcs.toString
  vcs.toString()
  vcj.toString
  vcj.toString()

  ccr.run
  ccr.run()
  ccr.toString
  ccr.toString()
  ccs.toString
  ccs.toString()
  ccj.toString
  ccj.toString()

  vccr.toString
  vccr.toString()
  vccs.toString
  vccs.toString()
  vccj.toString
  vccj.toString()
}
