                     trait Sam1S { def apply(x: Any): Any }
@FunctionalInterface trait Sam1J { def apply(x: Any): Any }

class TestBase {
  def meth1(x: Any) = ""
}

class Test extends TestBase {
  val t5a: Any => Any = meth1                   // ok
  val t5b: Sam1S      = meth1                   // ok, but warning
  val t5c: Sam1J      = meth1                   // ok
  val t5d: Any => Any = { val t = meth1   ; t } // error in 2.13, eta-expansion in 2.14
  val t5e: Any => Any = { val t = meth1 _ ; t } // ?/ok
}
