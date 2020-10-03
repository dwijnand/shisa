class TestBase {
  def methF0() = () => ""
}

class Test extends TestBase {
  val t1a: () => Any = methF0                // ok, eta-expansion
  val t1b: () => Any = { val t = methF0; t } // `()`-insert b/c no expected type
  val t1c: () => Any = methF0 _              // ok, explicit eta-expansion requested
  val t1d: Any       = methF0 _              // ok, explicit eta-expansion requested
  val t1e: Any       = methF0() _            // error: _ must follow method
}
