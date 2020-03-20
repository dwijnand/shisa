                     trait Sam0S { def apply(): Int }
@FunctionalInterface trait Sam0J { def apply(): Int }

                     trait Sam1S { def apply(x: Int): Int }
@FunctionalInterface trait Sam1J { def apply(x: Int): Int }

class A { def boom(): Unit }

object Defs {
  def foo()       = () => ""
  def bar         = ""
  def baz()       = ""
  def zap()()     = ""
  def zup(x: Int) = x

  def m1         = 1
  def m2()       = 1
  def m3(x: Int) = x

  val a = new A
}; import Defs._

class EtaExpandZeroArg { // -Xlint:eta-zero -Xlint:eta-sam
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
  val t5Fun: Int => Int = zup // ok
  val t5Sam1S: Sam1S    = zup // ok, but warning
  val t5Sam1J: Sam1J    = zup // ok
}

class EtaExpand214 { // -Xsource:2.14 {-Xlint:eta-zero -Xlint:eta-sam,-deprecation -Werror}
  val t2: () => Any  = m2   // warn/succ: eta-expanded (-Xlint:eta-zero)
  val t2Sam0S: Sam0S = m2   // warn/succ: eta-expanded (-Xlint:eta-zero + -Xlint:eta-sam)
  val t2Sam0J: Sam0J = m2   // warn/succ: eta-expanded (-Xlint:eta-zero)
  val t3: Int => Any = m3   // ok

  val t4 = m1 // apply
  val t5 = m2 // succ/warn: apply, ()-insertion
  val t6 = m3 // eta-expansion in 2.14

  val t4a: Int        = t4 // ok
  val t5a: Int        = t5 // ok
  val t6a: Int => Any = t6 // ok

  val t7 = m1 _
  val t8 = m2 _
  val t9 = m3 _

  val t7a: () => Any  = t7 // ok
  val t8a: () => Any  = t8 // ok
  val t9a: Int => Any = t9 // ok

  a.boom // ?/err: apply, ()-insertion

  import scala.collection.mutable.Map
  val xs = Map(1 -> "foo")
  val ys = xs.clone // ?/ok
}
