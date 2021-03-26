package shisa
package tests

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_negTests toSource")(compareToSource(Call.negTests, Call_negTests_expected))
  test("Call_posTests toSource")(compareToSource(Call.posTests, Call_posTests_expected))

  def Call_negTests_expected =
    """object Test {
      |  val str = ""
      |  def m1() = 1
      |  def m2 = 2
      |  str.##()
      |  m1
      |  m2()
      |}
      |""".stripMargin

  def Call_posTests_expected =
    """object Test {
      |  val str = ""
      |  class CR
      |  class CS { override def toString = "" }
      |  class CJ { override def toString() = "" }
      |  str.toString
      |  str.toString()
      |  new CR().toString
      |  new CR().toString()
      |  new CS().toString
      |  new CS().toString()
      |  new CJ().toString
      |  new CJ().toString()
      |  str.##
      |}
      |""".stripMargin

  def compareToSource(ts: List[TestContents], expected: String) = {
    assertEquals(Main.toObject(Test.toContents(ts)).syntax + "\n", expected)
  }
}
