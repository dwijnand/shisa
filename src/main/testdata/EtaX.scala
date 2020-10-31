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
  def typeMismatch2(obt: String, exp: String) = s"type mismatch;\n found   : $obt\n required: $exp"
  def typeMismatch3(obt: String, exp: String) = s"Found:    $obt\nRequired: $exp"
  def missingArgs(meth: String, cls: String) =
    s"""missing argument list for method $meth in class $cls
       |Unapplied methods are only converted to functions when a function type is expected.
       |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin
  def stillEta(meth: String, traitName: String) =
    s"method $meth is eta-expanded even though $traitName does not have the @FunctionalInterface annotation."
}

object EtaX {
  import ErrorMsgs._
  import MkInMemoryTestFile._

  object meth1 extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.meth1.lines.scala")
    def Sam1S        = q"                     trait Sam1S { def apply(x: Any): Any }"
    def Sam1J        = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"
    def outerDefns   = List(Sam1S, Sam1J)
    def baseClass    = q"""class TestBase { def meth1(x: Any) = "" }"""
    def testStats    = List(
      q"val t5a: Any => Any = meth1   // ok",
      q"val t5b: Sam1S      = meth1   // ok, but warning",
      q"val t5c: Sam1J      = meth1   // ok",
      q"val t5d             = meth1   // error in 2.13, eta-expansion in 3.0",
      q"val t5e             = meth1 _ // ok",
    )
    val path1        = Paths.get("testdata/EtaX/EtaX.meth1.01.scala")
    val path3        = Paths.get("testdata/EtaX/EtaX.meth1.03.scala")
    val  errs2       = List(err(path3, 13, missingArgs("meth1", "TestBase")))
    val warns3       = List(warn(path1, 11, stillEta("meth1", "p01.Sam1S")))
    val expectedMsgs = List(errs2, errs2, Nil, warns3, warns3, warns3, warns3)
    val contents     = TestContents(List(outerDefns), Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object prop extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.prop.lines.scala")
    def baseClass    = q"""class TestBase { def prop = "" }"""
    def testStats    = List(
      q"val t2a: () => Any = prop     // error: no eta-expansion of nullary methods",
      q"val t2b            = prop     // ok: apply",
      q"val t2c: () => Any = prop()   // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument",
      q"val t2d: () => Any = prop _   // ok",
      q"val t2e            = prop _   // ?/ok",
      q"val t2f: Any       = prop _   // ok",
      q"val t2g: Any       = prop() _ // error: not enough arguments for method apply",
    )
    val path0        = Paths.get("testdata/EtaX/EtaX.prop.00.scala")
    val errs2        = List(err(path0, 7, typeMismatch2("String", "() => Any")))
    val errs3        = List(err(path0, 7, typeMismatch3("String", "() => Any")))
    val expectedMsgs = List(errs2, errs2, errs2, errs3, errs3, errs3, errs3)
    val contents     = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object methF0 extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.methF0.lines.scala")
    def baseClass    = q"""class TestBase { def methF0() = () => "" }"""
    def testStats    = List(
      q"val t1a: () => Any = methF0     // ok, eta-expansion",
      q"val t1b            = methF0     // `()`-insert b/c no expected type",
      q"val t1c: () => Any = methF0 _   // ok, explicit eta-expansion requested",
      q"val t1d: Any       = methF0 _   // ok, explicit eta-expansion requested",
      q"val t1e: Any       = methF0() _ // error: _ must follow method",
    )
    val expectedMsgs = List(warns2, warns2, warns2, warns3, errs3, errs3, errs3)
    def path1        = Paths.get("testdata/EtaX/EtaX.methF0.01.scala")
    def warns2       = List(warn(path1, 8, autoApp2("methF0")))
    def warns3       = List(warn(path1, 8, parensCall3("methF0")))
    def  errs3       = List( err(path1, 8, parensCall3("methF0")))
    val contents     = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

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
