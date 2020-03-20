                     trait Sam0S { def apply(): Any }
@FunctionalInterface trait Sam0J { def apply(): Any }

                     trait Sam1S { def apply(x: Any): Any }
@FunctionalInterface trait Sam1J { def apply(x: Any): Any }

class A { def boom(): Unit }

class EtaExpandZeroArg { // -Xlint:eta-zero -Xlint:eta-sam
  def foo()       = () => ""
  def bar         = ""
  def baz()       = ""
  def zap()()     = ""
  def zup(x: Any) = ""

  val t1a: () => Any = foo()              // ok (obviously)
  val t1b: () => Any = foo                // eta-expansion, but lint warning
  val t1c: () => Any = { val t = foo; t } // `()`-insertion because no expected type
  val t1d: () => Any = foo _              // ok, explicit eta-expansion requested
  val t1e: Any       = foo _              // ok, explicit eta-expansion requested
  val t1f: Any       = foo() _            // error: _ must follow method

  val t2a: () => Any = bar     // error: no eta-expansion of zero-arglist-methods (nullary methods)
  val t2b: () => Any = bar()   // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument
  val t2c: () => Any = bar _   // ok
  val t2d: Any       = bar _   // ok
  val t2e: Any       = bar() _ // error: not enough arguments for method apply

  val t3a: () => Any = baz     // eta-expansion, but lint warning
  val t3b: () => Any = baz _   // ok
  val t3c: Any       = baz _   // ok
  val t3d: Any       = baz() _ // error: _ must follow method

  val t4a: () => Any = zap     // eta-expansion, but lint warning
  val t4b: () => Any = zap()   // ditto
  val t4c: () => Any = zap _   // ok
  val t4d: () => Any = zap() _ // ok

  val t5a               = zup // error in 2.13, eta-expansion in 2.14
  val t5Fun: Any => Any = zup // ok
  val t5Sam1S: Sam1S    = zup // ok, but warning
  val t5Sam1J: Sam1J    = zup // ok
}

class EtaExpand214 { // -Xsource:2.14 {-Xlint:eta-zero -Xlint:eta-sam,-deprecation -Werror}
  def bar         = ""
  def baz()       = ""
  def zup(x: Any) = x

  val t2: () => Any  = bar                  // warn/succ: eta-expanded (-Xlint:eta-zero)
  val t2Sam0S: Sam0S = bar                  // warn/succ: eta-expanded (-Xlint:eta-zero + -Xlint:eta-sam)
  val t2Sam0J: Sam0J = bar                  // warn/succ: eta-expanded (-Xlint:eta-zero)
  val t3: Any => Any = zup                  // ok
  val t4: Any        = { val t = bar; t }   // ok: apply
  val t5: Any        = { val t = baz; t }   // succ/warn: apply, ()-insertion
  val t6: Any => Any = { val t = zup; t }   // eta-expansion in 2.14
  val t7: () => Any  = { val t = bar _; t } // ok
  val t8: () => Any  = { val t = baz _; t } // ok
  val t9: Any => Any = { val t = zup _; t } // ok

  val a = new A
  a.boom // ?/err: apply, ()-insertion

  import scala.collection.mutable.Map
  val xs = Map(1 -> "foo")
  val ys = xs.clone // ok
}
