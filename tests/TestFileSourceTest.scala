package shisa
package tests

import scala.annotation.tailrec

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_negTests toSource")(compareToSource(Call.negTests, Call_negTests_expected))
  test("Call_posTests toSource")(compareToSource(Call.posTests, Call_posTests_expected))

  def Call_negTests_expected =
    """object Test {
      |  def m1() = 1
      |  def m2 = 2
      |  val any: Any = ""
      |  val ref: AnyRef = ""
      |  val obj: Object = ""
      |  val str: String = ""
      |  m1
      |  m2()
      |  any.##()
      |  ref.##()
      |  obj.##()
      |  str.##()
      |}
      |""".stripMargin

  def Call_posTests_expected =
    """object Test {
      |  val any: Any = ""
      |  val ref: AnyRef = ""
      |  val obj: Object = ""
      |  val str: String = ""
      |  class CR extends Runnable { def run() = () }
      |  class CS extends Runnable {
      |    def run() = ()
      |    override def toString = ""
      |  }
      |  class CJ extends Runnable {
      |    def run() = ()
      |    override def toString() = ""
      |  }
      |  case class CCR() extends Runnable { def run() = () }
      |  case class CCS() extends Runnable {
      |    def run() = ()
      |    override def toString = ""
      |  }
      |  case class CCJ() extends Runnable {
      |    def run() = ()
      |    override def toString() = ""
      |  }
      |  class VCR(val x: String) extends AnyVal
      |  class VCS(val x: String) extends AnyVal { override def toString = "" }
      |  class VCJ(val x: String) extends AnyVal { override def toString() = "" }
      |  case class VCCR(x: String) extends AnyVal
      |  case class VCCS(x: String) extends AnyVal { override def toString = "" }
      |  case class VCCJ(x: String) extends AnyVal { override def toString() = "" }
      |  any.##
      |  ref.##
      |  obj.##
      |  str.##
      |  any.toString
      |  any.toString()
      |  ref.toString
      |  ref.toString()
      |  obj.toString
      |  obj.toString()
      |  str.toString
      |  str.toString()
      |  any.getClass
      |  any.getClass()
      |  ref.getClass
      |  ref.getClass()
      |  obj.getClass
      |  obj.getClass()
      |  str.getClass
      |  str.getClass()
      |  any.hashCode
      |  any.hashCode()
      |  ref.hashCode
      |  ref.hashCode()
      |  obj.hashCode
      |  obj.hashCode()
      |  str.hashCode
      |  str.hashCode()
      |  new CR().toString
      |  new CR().toString()
      |  new CS().toString
      |  new CS().toString()
      |  new CJ().toString
      |  new CJ().toString()
      |  CCR().toString
      |  CCR().toString()
      |  CCS().toString
      |  CCS().toString()
      |  CCJ().toString
      |  CCJ().toString()
      |  new VCR("").toString
      |  new VCR("").toString()
      |  new VCS("").toString
      |  new VCS("").toString()
      |  new VCJ("").toString
      |  new VCJ("").toString()
      |  VCCR("").toString
      |  VCCR("").toString()
      |  VCCS("").toString
      |  VCCS("").toString()
      |  VCCJ("").toString
      |  VCCJ("").toString()
      |  new CR().run
      |  new CR().run()
      |  CCR().run
      |  CCR().run()
      |}
      |""".stripMargin

  def compareToSource(tf: TestFile, expected: String) = {
    assertEquals(Main.toObject(toContents(tf)).syntax + "\n", expected)
  }

  @tailrec final def toContents(test: shisa.Test): TestContents = test match {
    case x @ TestContents(_, _, _) => x
    case TestList(tests)           => Test.toContents(tests)
    case TestFile(_, test)         => toContents(test)
  }
}
