trait PU extends Any { def d   : String }
class P2M_VC(val x: String) extends AnyVal with PU { def d() = "" }

class Test {
  val p2m_vc = new P2M_VC("")
  p2m_vc.d()
}
