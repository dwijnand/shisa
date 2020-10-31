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
  def etaFunction                                          = "The syntax `<function> _` is no longer supported;\nyou can use `(() => <function>())` instead"
  def etaFunction2                                         = "The syntax `<function> _` is no longer supported;\nyou can simply leave out the trailing ` _`"
  def typeMismatch2(obt: String, exp: String) = s"type mismatch;\n found   : $obt\n required: $exp"
  def typeMismatch3(obt: String, exp: String) = s"Found:    $obt\nRequired: $exp"
  def missingArgs(meth: String, cls: String) =
    s"""missing argument list for method $meth in class $cls
       |Unapplied methods are only converted to functions when a function type is expected.
       |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin
  def stillEta(meth: String, traitName: String) =
    s"method $meth is eta-expanded even though $traitName does not have the @FunctionalInterface annotation."
  def mustFollow(tpe: String)  = s"_ must follow method; cannot follow $tpe"
  def mustParens(meth: String) = s"method $meth must be called with () argument"
  def onlyFuncs(tpe: String)   = s"Only function types can be followed by _ but the current expression has type $tpe"
  def notEnoughArgs(methsig: String, className: String, param: String) =
    s"""not enough arguments for method $methsig in class $className.
       |Unspecified value parameter $param.""".stripMargin
  def methodsWithoutParams    = "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, write a function literal `() => m` instead"
  def methodsWithoutParamsNew = "Methods without a parameter list and by-name params can not be converted to functions as `m _`, write a function literal `() => m` instead"
  def missingArgForParam = "missing argument for parameter i of method apply: (i: Int): Char"
}

object EtaX {
  import ErrorMsgs._
  import MkInMemoryTestFile._

  def tests = List(boom, meth2, cloneEta, methF0, prop, meth1, meth)

  object meth extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.meth.lines.scala")
    def Sam0S        = q"                     trait Sam0S { def apply(): Any }"
    def Sam0J        = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
    def outerDefns   = List(Sam0S, Sam0J)
    def baseClass    = q"""class TestBase { def meth() = "" }"""
    def testStats    = List(
      q"val t3a: () => Any = meth     // eta-expansion, but lint warning",
      q"val t3Sam0S: Sam0S = meth     // -Xlint:eta-zero + -Xlint:eta-sam",
      q"val t3Sam0J: Sam0J = meth     // -Xlint:eta-zero",
      q"val t3b            = meth     // apply, ()-insertion",
      q"val t3c: () => Any = meth _   // ok",
      q"val t3d: () => Any = meth _   // ok",
      q"val t3e: Any       = meth _   // ok",
      q"val t3f: Any       = meth() _ // error: _ must follow method",
    )
    def pathN(n: Int) = Paths.get(s"testdata/EtaX/EtaX.meth.0$n.scala")
    val errs2         = List(
       err(pathN(1), 11, typeMismatch2("String", "p01.Sam0S")),
       err(pathN(2), 12, typeMismatch2("String", "p02.Sam0J")),
      warn(pathN(3), 13, autoApp2("meth")),
       err(pathN(7), 17, mustFollow("String")),
    )
    val msgs3Old      = List(
      warn(pathN(1), 11, parensCall3("meth")),
       err(pathN(1), 11, typeMismatch3("String", "p01.Sam0S")),
      warn(pathN(2), 12, mustParens("meth")),
       err(pathN(2), 12, typeMismatch3("String", "p02.Sam0J")),
      warn(pathN(3), 13, mustParens("meth")),
      warn(pathN(7), 17, onlyFuncs("String")),
    )
    val msgs3         = List(
      err(pathN(1), 11, parensCall3("meth")),
      err(pathN(2), 12, parensCall3("meth")),
      err(pathN(3), 13, parensCall3("meth")),
      err(pathN(7), 17, onlyFuncs("String")),
    )
    val msgs31Migr    = List(
      err(pathN(1), 11, parensCall3("meth")),
      err(pathN(2), 12, parensCall3("meth")),
      err(pathN(3), 13, parensCall3("meth")),
      warn(pathN(4), 14, etaFunction),
      warn(pathN(5), 15, etaFunction),
      warn(pathN(6), 16, etaFunction),
      warn(pathN(7), 17, onlyFuncs("String")),
    )
    val msgs31        = List(
      err(pathN(1), 11, parensCall3("meth")),
      err(pathN(2), 12, parensCall3("meth")),
      err(pathN(3), 13, parensCall3("meth")),
      err(pathN(4), 14, etaFunction),
      err(pathN(5), 15, etaFunction),
      err(pathN(6), 16, etaFunction),
      err(pathN(7), 17, onlyFuncs("String")),
    )
    val expectedMsgs  = List(errs2, errs2, errs2, msgs3Old, msgs3, msgs31Migr, msgs31)
    val contents      = TestContents(List(outerDefns), Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

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
    def pathN(n: Int) = Paths.get(s"testdata/EtaX/EtaX.meth1.0$n.scala")
    val  errs2        = List( err(pathN(3), 13, missingArgs("meth1", "TestBase")))
    val warns3        = List(warn(pathN(1), 11, stillEta("meth1", "p01.Sam1S")))
    val msgs31Migr    = List(
      warn(pathN(1), 11, stillEta("meth1", "p01.Sam1S")),
      warn(pathN(4), 14, etaFunction2),
    )
    val msgs31        = List(
      warn(pathN(1), 11, stillEta("meth1", "p01.Sam1S")),
       err(pathN(4), 14, etaFunction2),
    )
    val expectedMsgs  = List(errs2, errs2, Nil, warns3, warns3, msgs31Migr, msgs31)
    val contents      = TestContents(List(outerDefns), Some(baseClass), Nil, List(testStats), expectedMsgs)
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
    def pathN(n: Int) = Paths.get(s"testdata/EtaX/EtaX.prop.0$n.scala")
    val msgs2         = List(
       err(pathN(0),  7, typeMismatch2("String", "() => Any")),
       err(pathN(2),  9, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
      warn(pathN(3), 10, methodsWithoutParams),
      warn(pathN(4), 11, methodsWithoutParams),
      warn(pathN(5), 12, methodsWithoutParams),
       err(pathN(6), 13, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
    )
    val msgs2New      = List(
      err(pathN(0),  7, typeMismatch2("String", "() => Any")),
      err(pathN(2),  9, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
      err(pathN(3), 10, methodsWithoutParamsNew),
      err(pathN(4), 11, methodsWithoutParamsNew),
      err(pathN(5), 12, methodsWithoutParamsNew),
      err(pathN(6), 13, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
    )
    val msgs3Old      = List(
       err(pathN(0),  7, typeMismatch3("String", "() => Any")),
       err(pathN(2),  9, missingArgForParam),
      warn(pathN(3), 10, onlyFuncs("String")),
      warn(pathN(4), 11, onlyFuncs("String")),
      warn(pathN(5), 12, onlyFuncs("String")),
      warn(pathN(6), 13, onlyFuncs("<error unspecified error>")),
       err(pathN(6), 13, missingArgForParam),
    )
    val msgs3         = List(
      err(pathN(0),  7, typeMismatch3("String", "() => Any")),
      err(pathN(2),  9, missingArgForParam),
      err(pathN(3), 10, onlyFuncs("String")),
      err(pathN(4), 11, onlyFuncs("String")),
      err(pathN(5), 12, onlyFuncs("String")),
      err(pathN(6), 13, onlyFuncs("<error unspecified error>")),
    )
    val msgs31Migr    = List(
       err(pathN(0),  7, typeMismatch3("String", "() => Any")),
       err(pathN(2),  9, missingArgForParam),
      warn(pathN(3), 10, onlyFuncs("String")),
       err(pathN(3), 10, typeMismatch3("String", "() => Any")),
      warn(pathN(4), 11, onlyFuncs("String")),
      warn(pathN(5), 12, onlyFuncs("String")),
      warn(pathN(6), 13, onlyFuncs("<error unspecified error>")),
       err(pathN(6), 13, missingArgForParam),
    )
    val msgs31        = List(
      err(pathN(0),  7, typeMismatch3("String", "() => Any")),
      err(pathN(2),  9, missingArgForParam),
      err(pathN(3), 10, onlyFuncs("String")),
      err(pathN(4), 11, onlyFuncs("String")),
      err(pathN(5), 12, onlyFuncs("String")),
      err(pathN(6), 13, onlyFuncs("<error unspecified error>")),
    )
    val expectedMsgs  = List(msgs2, msgs2, msgs2New, msgs3Old, msgs3, msgs31Migr, msgs31)
    val contents      = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
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
    def pathN(n: Int) = Paths.get(s"testdata/EtaX/EtaX.methF0.0$n.scala")
    def warns2        = List(
      warn(pathN(1),  8, autoApp2("methF0")),
       err(pathN(4), 11, mustFollow("() => String")),
    )
    def warns3        = List(
      warn(pathN(1),  8, parensCall3("methF0")),
      warn(pathN(4), 11, onlyFuncs("() => String")),
    )
    def  errs3        = List(
      err(pathN(1),  8, parensCall3("methF0")),
      err(pathN(4), 11, onlyFuncs("() => String")),
    )
    def  msgs31Migr   = List(
       err(pathN(1),  8, parensCall3("methF0")),
      warn(pathN(2),  9, etaFunction),
      warn(pathN(3), 10, etaFunction),
      warn(pathN(4), 11, onlyFuncs("() => String")),
    )
    def  msgs31       = List(
      err(pathN(1),  8, parensCall3("methF0")),
      err(pathN(2),  9, etaFunction),
      err(pathN(3), 10, etaFunction),
      err(pathN(4), 11, onlyFuncs("() => String")),
    )
    val expectedMsgs  = List(warns2, warns2, warns2, warns3, errs3, msgs31Migr, msgs31)
    val contents      = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
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
    def pathN(n: Int) = Paths.get(s"testdata/EtaX/EtaX.meth2.0$n.scala")
    val errs31Migr    = List(
      warn(pathN(0),  7, etaFunction),
      warn(pathN(2),  9, etaFunction),
      warn(pathN(3), 10, etaFunction),
    )
    val errs31        = List(
      err(pathN(0),  7, etaFunction),
      err(pathN(2),  9, etaFunction),
      err(pathN(3), 10, etaFunction),
    )
    val expectedMsgs  = List(Nil, Nil, Nil, Nil, Nil, errs31Migr, errs31)
    val contents      = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
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
