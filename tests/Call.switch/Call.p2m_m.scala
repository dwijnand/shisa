trait P { def d   : String }
class P2M extends P { def d() = "" }

class Test {
  val p2m = new P2M
  p2m.d()
}
