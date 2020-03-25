class TestBase {
  def prop = ""
}

class Test extends TestBase {
  val t2a: () => Any = prop                   // error: no eta-expansion of nullary methods
  val t2b: Any       = { val t = prop   ; t } // ok: apply
  val t2c: () => Any = prop()                 // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument
  val t2d: () => Any = prop _                 // ok
  val t2e: () => Any = { val t = prop _ ; t } // ?/ok
  val t2f: Any       = prop _                 // ok
  val t2g: Any       = prop() _               // error: not enough arguments for method apply
}
