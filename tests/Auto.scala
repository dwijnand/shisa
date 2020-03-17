case class Address()

class A() { override def toString() : String = "A()" }
class B() { override def toString   : String = "B()" }

case class C(c: Int)      extends AnyVal                                  // generates this.underlying.hashCode
class Bibbity(val i: Int) extends AnyVal { override def toString = "hi" } // generates toString$extension

class City extends Runnable { override def run(): Unit = () }
object City { val c           = new City; c.run } // should be ok without parens
object Sam  { val r: Runnable = () => (); r.run } // should be ok without parens

class AutoApplication { // -Werror -deprecation -Xsource:2.14
  def a1(xs: List[String]): Int      = xs.hashCode
  def a2(xs: List[String]): Int      = xs.hashCode()
  def a3(xs: List[String]): String   = xs.toString
  def a4(xs: List[String]): String   = xs.toString()
  def a5(xs: List[String]): Class[_] = xs.getClass
  def a6(xs: List[String]): Class[_] = xs.getClass()
  def a7(xs: List[String]): Int      = xs.##
  def a8(xs: List[String]): Int      = xs.##()
  def a9(x: Address): String         = x.toString
  def a10(x: Address): String        = x.toString()
  def a11(x: A): String              = x.toString
  def a12(x: A): String              = x.toString()
  def a13(x: B): String              = x.toString
  def a14(x: B): String              = x.toString()
}
