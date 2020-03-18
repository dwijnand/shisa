trait PU extends Any { def d   : String }
trait MU extends Any { def d() : String }
class P2M_VC(val x: String) extends AnyVal with PU { def d() = "" }
class M2P_VC(val x: String) extends AnyVal with MU { def d   = "" }

class Test {
  val p2m_vc = new P2M_VC("")
  val m2p_vc = new M2P_VC("")

  p2m_vc.d
  p2m_vc.d()
  m2p_vc.d
  m2p_vc.d()
}
