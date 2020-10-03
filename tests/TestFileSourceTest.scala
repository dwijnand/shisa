package shisa
package tests

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_## toSource") {
    val testFile = testdata.Call_##.testFile
    val obtained = scala.meta.ShisaMeta.testFileSource(testFile)
    val expected =
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
    assertEquals(obtained, expected)
  }
}
