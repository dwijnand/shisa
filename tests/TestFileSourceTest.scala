package shisa
package tests

import shisa.testdata._

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_## toSource")(compareToSource(Call_##.testFile, Call_hashhash_expected))
  test("Call_pos toSource")(compareToSource(Call_pos.testFile, Call_pos_expected))

  def Call_hashhash_expected =
    """class Test {
      |  val any: Any = ""
      |  val ref: AnyRef = ""
      |
      |  any.##
      |  any.##()
      |
      |  ref.##
      |  ref.##()
      |}
      |""".stripMargin

  def Call_pos_expected =
    """class CR extends Runnable { def run() = () }
      |class CS { override def toString = "" }
      |class CJ { override def toString() = "" }
      |
      |case class CCR() extends Runnable { def run() = () }
      |case class CCS() { override def toString = "" }
      |case class CCJ() { override def toString() = "" }
      |
      |class VCR(val x: String) extends AnyVal
      |class VCS(val x: String) extends AnyVal { override def toString = "" }
      |class VCJ(val x: String) extends AnyVal { override def toString() = "" }
      |
      |case class VCCR(x: String) extends AnyVal
      |case class VCCS(x: String) extends AnyVal { override def toString = "" }
      |case class VCCJ(x: String) extends AnyVal { override def toString() = "" }
      |
      |class Test {
      |  val any: Any = ""
      |  val ref: AnyRef = ""
      |  val obj: Object = ""
      |  val str: String = ""
      |
      |  any.##
      |  any.getClass
      |  any.getClass()
      |  any.hashCode
      |  any.hashCode()
      |  any.toString
      |  any.toString()
      |
      |  ref.##
      |  ref.getClass
      |  ref.getClass()
      |  ref.hashCode
      |  ref.hashCode()
      |  ref.toString
      |  ref.toString()
      |
      |  obj.##
      |  obj.getClass
      |  obj.getClass()
      |  obj.hashCode
      |  obj.hashCode()
      |  obj.toString
      |  obj.toString()
      |
      |  str.##
      |  str.getClass
      |  str.getClass()
      |  str.hashCode
      |  str.hashCode()
      |  str.toString
      |  str.toString()
      |
      |  new CR().run
      |  new CR().run()
      |  new CR().toString
      |  new CR().toString()
      |  new CS().toString
      |  new CS().toString()
      |  new CJ().toString
      |  new CJ().toString()
      |
      |  new VCR("").toString
      |  new VCR("").toString()
      |  new VCS("").toString
      |  new VCS("").toString()
      |  new VCJ("").toString
      |  new VCJ("").toString()
      |
      |  CCR().run
      |  CCR().run()
      |  CCR().toString
      |  CCR().toString()
      |  CCS().toString
      |  CCS().toString()
      |  CCJ().toString
      |  CCJ().toString()
      |
      |  VCCR("").toString
      |  VCCR("").toString()
      |  VCCS("").toString
      |  VCCS("").toString()
      |  VCCJ("").toString
      |  VCCJ("").toString()
      |}
      |""".stripMargin

  def compareToSource(testFile: InMemoryTestFile, expected: String) = {
    val obtained = scala.meta.ShisaMeta.testFileSource(testFile)
    assertEquals(obtained, expected)
  }
}
