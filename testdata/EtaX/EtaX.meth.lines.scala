                     trait Sam0S { def apply(): Any }
@FunctionalInterface trait Sam0J { def apply(): Any }

class TestBase {
  def meth() = ""
}

class Test extends TestBase {
  val t3a: () => Any = meth                   // eta-expansion, but lint warning
  val t3Sam0S: Sam0S = meth                   // -Xlint:eta-zero + -Xlint:eta-sam
  val t3Sam0J: Sam0J = meth                   // -Xlint:eta-zero
  val t3b: Any       = { val t = meth   ; t } // apply, ()-insertion
  val t3c: () => Any = meth _                 // ok
  val t3d: () => Any = { val t = meth _ ; t } // ok
  val t3e: Any       = meth _                 // ok
  val t3f: Any       = meth() _               // error: _ must follow method
}
