trait MU extends Any { def d() : String }
class M2P_VC(val x: String) extends AnyVal with MU { def d   = "" }

class Test {
  val m2p_vc = new M2P_VC("")
  m2p_vc.d
}
