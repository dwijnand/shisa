package shisa
package testdata

import java.nio.file._

import scala.meta._, contrib._

object ErrorMsgs {
  val str1 = "(): String"
  val str2 = "=> String"
  def autoApp2(meth: String) =
    s"""Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method $meth,
       |or remove the empty argument list from its definition (Java-defined methods are exempt).
       |In Scala 3, an unapplied method like this will be eta-expanded into a function.""".stripMargin
  def parensCall3(meth: String) = s"method $meth must be called with () argument"
  def p2mMsg = "method with a single empty parameter list overrides method without any parameter list"
  def p2mErr(nme: String) = s"$p2mMsg\ndef d: String (defined in trait $nme)"
  def errOverride2                                         = "method without a parameter list overrides a method with a single empty one"
  def errOverride3A(nme: String, tp1: String, tp2: String) = s"error overriding method d in trait $nme of type $tp1;\n  method d of type $tp2 no longer has compatible type"
  def errOverride3B(nme: String, tp1: String, tp2: String) = s"error overriding method d in trait $nme of type $tp1;\n  method d of type $tp2 has incompatible type"
  def etaFunction                                          =
    """The syntax `<function> _` is no longer supported;
      |you can use `(() => <function>())` instead""".stripMargin
}

object EtaX {
  import ErrorMsgs._
  import MkInMemoryTestFile._

  object cloneEta extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.clone.lines.scala")
    def baseClass    = q"""class TestBase { val t  = scala.collection.mutable.Map(1 -> "foo") }"""
    def testStat     = q"""val ys = t.clone"""
    val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, Nil, Nil)
    val contents     = TestContents(Nil, Some(baseClass), Nil, List(List(testStat)), expectedMsgs)
  }

  object meth2 extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.meth2.lines.scala")
    val baseClass    = q"""class TestBase { def meth2()() = "" }"""
    val testStats    = List(
      q"val t4a: () => Any = meth2     // eta-expansion, but lint warning",
      q"val t4b: () => Any = meth2()   // ditto",
      q"val t4c: () => Any = meth2 _   // ok",
      q"val t4d: () => Any = meth2() _ // ok",
    )
    val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, errs31Migr, errs31)
    def path0        = Paths.get("testdata/EtaX/EtaX.meth2.00.scala")
    def  errs31Migr  = List(warn(path0, 7, etaFunction))
    def  errs31      = List( err(path0, 7, etaFunction))
    val contents     = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object boom extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.boom.lines.scala")
    val outerDefn    = q"class A { def boom(): Unit = () }"
    val testStat     = q"new A().boom" // ?/?/err: apply, ()-insertion
    val expectedMsgs = List(warns2, warns2, warns2, warns3, errs3, errs3, errs3)
    def path0        = Paths.get("testdata/EtaX/EtaX.boom.00.scala")
    def warns2       = List(warn(path0, 7, autoApp2("boom")))
    def warns3       = List(warn(path0, 7, parensCall3("boom")))
    def  errs3       = List( err(path0, 7, parensCall3("boom")))
    val contents     = TestContents(List(List(outerDefn)), None, Nil, List(List(testStat)), expectedMsgs)
  }
}
