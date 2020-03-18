trait M { def d() : String }
class M2P extends M { def d   = "" }

class Test {
  val m2p = new M2P
  m2p.d
}
