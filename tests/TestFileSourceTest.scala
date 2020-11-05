package shisa
package tests

import shisa.testdata._

import munit._

class TestFileSourceTest extends FunSuite {
  test("Call_## toSource")(compareToSource(Call.hashHash, Call_hashhash_expected))
  test("Call_pos toSource")(compareToSource(Call.pos, Call_pos_expected))

  def Call_hashhash_expected =
    """object Test {
      |  val any: Any = ""
      |  val ref: AnyRef = ""
      |  val obj: Object = ""
      |  val str: String = ""
      |  any.##
      |  any.##()
      |  ref.##
      |  ref.##()
      |  obj.##
      |  obj.##()
      |  str.##
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
      |  any.getClass
      |  any.getClass()
      |  any.hashCode
      |  any.hashCode()
      |  any.toString
      |  any.toString()
      |  ref.getClass
      |  ref.getClass()
      |  ref.hashCode
      |  ref.hashCode()
      |  ref.toString
      |  ref.toString()
      |  obj.getClass
      |  obj.getClass()
      |  obj.hashCode
      |  obj.hashCode()
      |  obj.toString
      |  obj.toString()
      |  str.getClass
      |  str.getClass()
      |  str.hashCode
      |  str.hashCode()
      |  str.toString
      |  str.toString()
      |  new CR().run
      |  new CR().run()
      |  new CR().toString
      |  new CR().toString()
      |  new CS().toString
      |  new CS().toString()
      |  new CJ().toString
      |  new CJ().toString()
      |  new VCR("").toString
      |  new VCR("").toString()
      |  new VCS("").toString
      |  new VCS("").toString()
      |  new VCJ("").toString
      |  new VCJ("").toString()
      |  CCR().run
      |  CCR().run()
      |  CCR().toString
      |  CCR().toString()
      |  CCS().toString
      |  CCS().toString()
      |  CCJ().toString
      |  CCJ().toString()
      |  VCCR("").toString
      |  VCCR("").toString()
      |  VCCS("").toString
      |  VCCS("").toString()
      |  VCCJ("").toString
      |  VCCJ("").toString()
      |}
      |""".stripMargin

  def compareToSource(mk: MkInMemoryTestFile, expected: String) = {
    val obtained = Main.toSource(mk.contents)
    assertEquals(obtained, expected)
  }
}
