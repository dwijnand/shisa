package shisa
package tests

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_## toSource")(compareToSource(Call.hashHash.contents, Call_hashhash_expected))
  test("Call_pos toSource")(compareToSource(Call.pos.contents, Call_pos_expected))

  def Call_hashhash_expected =
    """object Test {
      |  val any: Any = ""
      |  val ref: AnyRef = ""
      |  val obj: Object = ""
      |  val str: String = ""
      |  any.##()
      |  ref.##()
      |  obj.##()
      |  str.##()
      |}
      |""".stripMargin

  def Call_pos_expected =
    """object Test {
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
      |  val any: Any = ""
      |  val ref: AnyRef = ""
      |  val obj: Object = ""
      |  val str: String = ""
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

  def compareToSource(contents: TestContents, expected: String) = {
    val obtained = Main.toSource(contents)
    assertEquals(obtained, expected)
  }
}
