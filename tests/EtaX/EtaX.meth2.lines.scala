class TestBase {
  def meth2()() = ""
}

class Test extends TestBase {
  val t4a: () => Any = meth2     // eta-expansion, but lint warning
  val t4b: () => Any = meth2()   // ditto
  val t4c: () => Any = meth2 _   // ok
  val t4d: () => Any = meth2() _ // ok
}
