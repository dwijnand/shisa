                     trait Sam0S { def apply(): Any }
@FunctionalInterface trait Sam0J { def apply(): Any }

                     trait Sam1S { def apply(x: Any): Any }
@FunctionalInterface trait Sam1J { def apply(x: Any): Any }

trait A { def boom(): Unit }

class TestBase {
  def foo()       = () => ""
  def bar         = ""
  def baz()       = ""
  def zap()()     = ""
  def zup(x: Any) = ""
}

class Test extends TestBase {
  // [1]:               -Xlint:eta-zero -Xlint:eta-sam
  // [2]: -Xsource:2.14 -Xlint:eta-zero -Xlint:eta-sam
  // [3]: -Xsource:2.14 -deprecation -Werror

  val t1a: () => Any = foo                // ok, eta-expansion
  val t1b: () => Any = { val t = foo; t } // `()`-insert b/c no expected type
  val t1c: () => Any = foo _              // ok, explicit eta-expansion requested
  val t1d: Any       = foo _              // ok, explicit eta-expansion requested
  val t1e: Any       = foo() _            // error: _ must follow method

  val t2a: () => Any = bar                   // error: no eta-expansion of nullary methods
  val t2b: Any       = { val t = bar   ; t } // ok: apply
  val t2c: () => Any = bar()                 // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument
  val t2d: () => Any = bar _                 // ok
  val t2e: () => Any = { val t = bar _ ; t } // ?/ok
  val t2f: Any       = bar _                 // ok
  val t2g: Any       = bar() _               // error: not enough arguments for method apply

  val t3a: () => Any = baz                   // eta-expansion, but lint warning
  val t2Sam0S: Sam0S = baz                   // err??/warn/succ: -Xlint:eta-zero + -Xlint:eta-sam, no eta-expansion w/o 2.14?
  val t2Sam0J: Sam0J = baz                   // err??/warn/succ: -Xlint:eta-zero, no eta-expansion w/o 2.14?
  val t3b: Any       = { val t = baz   ; t } // ?/succ/warn: apply, ()-insertion
  val t3a: () => Any = baz _                 // ok
  val t3c: () => Any = { val t = baz _ ; t } // ok?/ok
  val t3a: Any       = baz _                 // ok
  val t3d: Any       = baz() _               // error: _ must follow method

  val t4a: () => Any = zap     // eta-expansion, but lint warning
  val t4b: () => Any = zap()   // ditto
  val t4c: () => Any = zap _   // ok
  val t4d: () => Any = zap() _ // ok

  val t5b: Any => Any = zup                   // ok
  val t5c: Sam1S      = zup                   // ok, but warning
  val t5d: Sam1J      = zup                   // ok
  val t5e: Any => Any = { val t = zup   ; t } // error in 2.13, eta-expansion in 2.14
  val t5f: Any => Any = { val t = zup _ ; t } // ?/ok

  new A().boom // ?/?/err: apply, ()-insertion

  val ys = { val t = scala.collection.mutable.Map(1 -> "foo"); xs.clone } // ?/?/ok
}
