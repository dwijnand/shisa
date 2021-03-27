package shisa
package tests

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_posTests toSource")(compareToSource(Call.posTests, Call_posTests_expected))
  test("Call_negTests toSource")(compareToSource(Call.negTests, Call_negTests_expected))

  def Call_posTests_expected =
    """object Test {
      |  class A {
      |    def m() = 1
      |    def p = 2
      |  }
      |  val a = new A()
      |  class S { override def toString = "" }
      |  val s = new S()
      |  class J { override def toString() = "" }
      |  val j = new J()
      |  a.toString
      |  s.toString
      |  s.toString()
      |  j.toString
      |}
      |""".stripMargin

  def Call_negTests_expected =
    """object Test {
      |  class A {
      |    def m() = 1
      |    def p = 2
      |  }
      |  val a = new A()
      |  a.m
      |  a.p()
      |  a.##()
      |}
      |""".stripMargin

  def compareToSource(ts: List[TestContents], expected: String) = {
    assertEquals(Main.toObject(Test.toContents(ts)).syntax + "\n", expected)
  }
}
