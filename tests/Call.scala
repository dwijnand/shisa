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

trait P { def d   : String }
trait M { def d() : String }
class P2M extends P { def d() = "" }
class M2P extends M { def d   = "" }

trait PU extends Any { def d   : String }
trait MU extends Any { def d() : String }
class P2M_VC(val x: String) extends AnyVal with PU { def d() = "" }
class M2P_VC(val x: String) extends AnyVal with MU { def d   = "" }

class Test {
  def meth() = ""
  def prop   = ""

  meth
  meth()
  prop
  prop()

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

  val p2m = new P2M
  val m2p = new P2M
  val p2m_vc = new P2M_VC("")
  val m2p_vc = new P2M_VC("")

  any.##
  any.##()
  any.getClass
  any.getClass()
  any.hashCode
  any.hashCode()
  any.toString
  any.toString()

  ref.##
  ref.##()
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

  p2m.d
  p2m.d()
  m2p.d
  m2p.d()

  p2m_vc.d
  p2m_vc.d()
  m2p_vc.d
  m2p_vc.d()
}
