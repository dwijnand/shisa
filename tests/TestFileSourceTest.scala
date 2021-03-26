package shisa
package tests

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_posTests toSource")(compareToSource(Call.posTests, Call_posTests_expected))
  test("Call_negTests toSource")(compareToSource(Call.negTests, Call_negTests_expected))

  def Call_posTests_expected =
    """object Test {
      |  val str = ""
      |  class CS { override def toString = "" }
      |  val cs = new CS()
      |  class CJ { override def toString() = "" }
      |  val cj = new CJ()
      |  str.toString
      |  str.toString()
      |  str.##
      |  cs.toString
      |  cs.toString()
      |  cj.toString
      |  cj.toString()
      |}
      |""".stripMargin

  def Call_negTests_expected =
    """object Test {
      |  val str = ""
      |  def m() = 1
      |  def p = 2
      |  str.##()
      |  m
      |  p()
      |}
      |""".stripMargin

  def compareToSource(ts: List[TestContents], expected: String) = {
    assertEquals(Main.toObject(Test.toContents(ts)).syntax + "\n", expected)
  }
}
