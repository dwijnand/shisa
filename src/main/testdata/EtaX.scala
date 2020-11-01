package shisa
package testdata

import java.nio.file._

import scala.meta._

import Severity.{ Warn, Error }

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
  def errOverride2 = "method without a parameter list overrides a method with a single empty one"
  def errOverride31(nme: String, tp1: String) = s"error overriding method d in trait $nme of type $tp1"
  def errOverride3A(nme: String, tp1: String, tp2: String) = errOverride31(nme, tp1) + s";\n  method d of type $tp2 no longer has compatible type"
  def errOverride3B(nme: String, tp1: String, tp2: String) = errOverride31(nme, tp1) + s";\n  method d of type $tp2 has incompatible type"
  def etaFunction  = "The syntax `<function> _` is no longer supported;\nyou can use `(() => <function>())` instead"
  def etaFunction2 = "The syntax `<function> _` is no longer supported;\nyou can simply leave out the trailing ` _`"
  def typeMismatch2(obt: String, exp: String) = s"type mismatch;\n found   : $obt\n required: $exp"
  def typeMismatch3(obt: String, exp: String) = s"Found:    $obt\nRequired: $exp"
  def missingArgs(meth: String, cls: String) =
    s"""missing argument list for method $meth in class $cls
       |Unapplied methods are only converted to functions when a function type is expected.
       |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin
  def stillEta(meth: String, traitName: String) = s"method $meth is eta-expanded even though $traitName does not have the @FunctionalInterface annotation."
  def mustFollow(tpe: String)  = s"_ must follow method; cannot follow $tpe"
  def mustParens(meth: String) = s"method $meth must be called with () argument"
  def onlyFuncs(tpe: String)   = s"Only function types can be followed by _ but the current expression has type $tpe"
  def methodsWithoutParams    = "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, write a function literal `() => m` instead"
  def methodsWithoutParamsNew = "Methods without a parameter list and by-name params can not be converted to functions as `m _`, write a function literal `() => m` instead"
  def notEnoughArgs(methsig: String, className: String, param: String) = s"not enough arguments for method $methsig in class $className.\nUnspecified value parameter $param."
  def missingArgForParam(methsig: String, param: String)               = s"missing argument for parameter $param of method $methsig"
}

object EtaX {
  import ErrorMsgs._
  import MkInMemoryTestFile._

  def tests = List(boom, meth2, cloneEta, methF0, prop, meth1, meth)

  object meth extends MkInMemoryTestFile {
    val path          = Paths.get("EtaX/EtaX.meth.lines.scala")
    def Sam0S         = q"                     trait Sam0S { def apply(): Any }"
    def Sam0J         = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
    def outerDefns    = List(Sam0S, Sam0J)
    def baseClass     = q"""class TestBase { def meth() = "" }"""
    def testStats     = List(
      q"val t3a: () => Any = meth     // eta-expansion, but lint warning",
      q"val t3Sam0S: Sam0S = meth     // -Xlint:eta-zero + -Xlint:eta-sam",
      q"val t3Sam0J: Sam0J = meth     // -Xlint:eta-zero",
      q"val t3b            = meth     // apply, ()-insertion",
      q"val t3c: () => Any = meth _   // ok",
      q"val t3d: () => Any = meth _   // ok",
      q"val t3e: Any       = meth _   // ok",
      q"val t3f: Any       = meth() _ // error: _ must follow method",
    )

    val msgs2         = List(
       err(Paths.get("EtaX/EtaX.meth.01.scala"), 9, typeMismatch2("String", "p01.Sam0S")),
       err(Paths.get("EtaX/EtaX.meth.02.scala"), 9, typeMismatch2("String", "p02.Sam0J")),
      warn(Paths.get("EtaX/EtaX.meth.03.scala"), 9, autoApp2("meth")),
       err(Paths.get("EtaX/EtaX.meth.07.scala"), 9, mustFollow("String")),
    )
    def msgs3Pair(sev: Severity, path: Path, lineNo: Int, exp: String) = List(
      msg(sev, path, lineNo, parensCall3("meth")),
    ) ::: (if (sev == Error) Nil else List(
      err(     path, lineNo, typeMismatch3("String", exp)),
    ))
    def msgs3I(sev: Severity) =
      msgs3Pair(sev, Paths.get("EtaX/EtaX.meth.01.scala"), 9, "p01.Sam0S") :::
      msgs3Pair(sev, Paths.get("EtaX/EtaX.meth.02.scala"), 9, "p02.Sam0J") ::: List(
      msg(      sev, Paths.get("EtaX/EtaX.meth.03.scala"), 9, mustParens("meth")),
      msg(      sev, Paths.get("EtaX/EtaX.meth.07.scala"), 9, onlyFuncs("String")),
    )
    def msgs31I(sev: Severity) = List(
      err(     Paths.get("EtaX/EtaX.meth.01.scala"), 9, parensCall3("meth")),
      err(     Paths.get("EtaX/EtaX.meth.02.scala"), 9, parensCall3("meth")),
      err(     Paths.get("EtaX/EtaX.meth.03.scala"), 9, parensCall3("meth")),
      msg(sev, Paths.get("EtaX/EtaX.meth.04.scala"), 9, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.meth.05.scala"), 9, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.meth.06.scala"), 9, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.meth.07.scala"), 9, onlyFuncs("String")),
    )

    val expectedMsgs  = List(msgs2, msgs2, msgs2, msgs3I(Warn), msgs3I(Error), msgs31I(Warn), msgs31I(Error))
    val contents      = TestContents(List(outerDefns), Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object meth1 extends MkInMemoryTestFile {
    val path          = Paths.get("EtaX/EtaX.meth1.lines.scala")
    def Sam1S         = q"                     trait Sam1S { def apply(x: Any): Any }"
    def Sam1J         = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"
    def outerDefns    = List(Sam1S, Sam1J)
    def baseClass     = q"""class TestBase { def meth1(x: Any) = "" }"""
    def testStats     = List(
      q"val t5a: Any => Any = meth1   // ok",
      q"val t5b: Sam1S      = meth1   // ok, but warning",
      q"val t5c: Sam1J      = meth1   // ok",
      q"val t5d             = meth1   // error in 2.13, eta-expansion in 3.0",
      q"val t5e             = meth1 _ // ok",
    )

    val msgs2                  = List( err(Paths.get("EtaX/EtaX.meth1.03.scala"), 9, missingArgs("meth1", "TestBase")))
    val msgs3                  = List(warn(Paths.get("EtaX/EtaX.meth1.01.scala"), 9, stillEta("meth1", "p01.Sam1S")))
    def msgs31I(sev: Severity) = msgs3 ::: List(msg(sev, Paths.get("EtaX/EtaX.meth1.04.scala"), 9, etaFunction2))

    val expectedMsgs = List(msgs2, msgs2, Nil, msgs3, msgs3, msgs31I(Warn), msgs31I(Error))
    val contents     = TestContents(List(outerDefns), Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object prop extends MkInMemoryTestFile {
    val path         = Paths.get("EtaX/EtaX.prop.lines.scala")
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

    def msgs2(sev: Severity)  = List(
      err(     Paths.get("EtaX/EtaX.prop.00.scala"), 6, typeMismatch2("String", "() => Any")),
      err(     Paths.get("EtaX/EtaX.prop.02.scala"), 6, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
      msg(sev, Paths.get("EtaX/EtaX.prop.03.scala"), 6, if (sev == Error) methodsWithoutParamsNew else methodsWithoutParams),
      msg(sev, Paths.get("EtaX/EtaX.prop.04.scala"), 6, if (sev == Error) methodsWithoutParamsNew else methodsWithoutParams),
      msg(sev, Paths.get("EtaX/EtaX.prop.05.scala"), 6, if (sev == Error) methodsWithoutParamsNew else methodsWithoutParams),
      err(     Paths.get("EtaX/EtaX.prop.06.scala"), 6, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
    )
    def msgs3(sev: Severity)  = List(
      err(     Paths.get("EtaX/EtaX.prop.00.scala"), 6, typeMismatch3("String", "() => Any")),
      err(     Paths.get("EtaX/EtaX.prop.02.scala"), 6, missingArgForParam("apply: (i: Int): Char", "i")),
      msg(sev, Paths.get("EtaX/EtaX.prop.03.scala"), 6, onlyFuncs("String")),
      msg(sev, Paths.get("EtaX/EtaX.prop.04.scala"), 6, onlyFuncs("String")),
      msg(sev, Paths.get("EtaX/EtaX.prop.05.scala"), 6, onlyFuncs("String")),
      msg(sev, Paths.get("EtaX/EtaX.prop.06.scala"), 6, onlyFuncs("<error unspecified error>")),
    ) ::: (if (sev == Error) Nil else List(
      err(     Paths.get("EtaX/EtaX.prop.06.scala"), 6, missingArgForParam("apply: (i: Int): Char", "i"))
    ))
    def msgs31(sev: Severity) = List(
      err(     Paths.get("EtaX/EtaX.prop.00.scala"), 6, typeMismatch3("String", "() => Any")),
      err(     Paths.get("EtaX/EtaX.prop.02.scala"), 6, missingArgForParam("apply: (i: Int): Char", "i")),
      msg(sev, Paths.get("EtaX/EtaX.prop.03.scala"), 6, onlyFuncs("String")),
    ) ::: (if (sev == Error) Nil else List(
      err(     Paths.get("EtaX/EtaX.prop.03.scala"), 6, typeMismatch3("String", "() => Any")),
    )) ::: List(
      msg(sev, Paths.get("EtaX/EtaX.prop.04.scala"), 6, onlyFuncs("String")),
      msg(sev, Paths.get("EtaX/EtaX.prop.05.scala"), 6, onlyFuncs("String")),
      msg(sev, Paths.get("EtaX/EtaX.prop.06.scala"), 6, onlyFuncs("<error unspecified error>")),
    ) ::: (if (sev == Error) Nil else List(
      err(Paths.get("EtaX/EtaX.prop.06.scala"), 6, missingArgForParam("apply: (i: Int): Char", "i")),
    ))

    val expectedMsgs = List(msgs2(Warn), msgs2(Warn), msgs2(Error), msgs3(Warn), msgs3(Error), msgs31(Warn), msgs31(Error))
    val contents     = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object methF0 extends MkInMemoryTestFile {
    val path          = Paths.get("EtaX/EtaX.methF0.lines.scala")
    def baseClass     = q"""class TestBase { def methF0() = () => "" }"""
    def testStats     = List(
      q"val t1a: () => Any = methF0     // ok, eta-expansion",
      q"val t1b            = methF0     // `()`-insert b/c no expected type",
      q"val t1c: () => Any = methF0 _   // ok, explicit eta-expansion requested",
      q"val t1d: Any       = methF0 _   // ok, explicit eta-expansion requested",
      q"val t1e: Any       = methF0() _ // error: _ must follow method",
    )
    def msgs2         = List(
      warn(Paths.get("EtaX/EtaX.methF0.01.scala"), 6, autoApp2("methF0")),
       err(Paths.get("EtaX/EtaX.methF0.04.scala"), 6, mustFollow("() => String")),
    )
    def msgs30(sev: Severity) = List(
      msg(sev, Paths.get("EtaX/EtaX.methF0.01.scala"), 6, parensCall3("methF0")),
      msg(sev, Paths.get("EtaX/EtaX.methF0.04.scala"), 6, onlyFuncs("() => String")),
    )
    def msgs31(sev: Severity) = List(
      err(     Paths.get("EtaX/EtaX.methF0.01.scala"), 6, parensCall3("methF0")),
      msg(sev, Paths.get("EtaX/EtaX.methF0.02.scala"), 6, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.methF0.03.scala"), 6, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.methF0.04.scala"), 6, onlyFuncs("() => String")),
    )
    val expectedMsgs  = List(msgs2, msgs2, msgs2, msgs30(Warn), msgs30(Error), msgs31(Warn), msgs31(Error))
    val contents      = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object cloneEta extends MkInMemoryTestFile {
    val path         = Paths.get("EtaX/EtaX.clone.lines.scala")
    def baseClass    = q"""class TestBase { val t  = scala.collection.mutable.Map(1 -> "foo") }"""
    def testStat     = q"""val ys = t.clone"""
    val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, Nil, Nil)
    val contents     = TestContents(Nil, Some(baseClass), Nil, List(List(testStat)), expectedMsgs)
  }

  object meth2 extends MkInMemoryTestFile {
    val path         = Paths.get("EtaX/EtaX.meth2.lines.scala")
    val baseClass    = q"""class TestBase { def meth2()() = "" }"""

    def testCase(stat: Stat, msgs: Severity => List[Msg]) = {
      val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, msgs(Warn), msgs(Error))
      TestContents(Nil, Some(baseClass), Nil, List(List(stat)), expectedMsgs)
    }

    val contents = List(
      testCase(q"val t4a: () => Any = meth2",     sev => List(msg(sev, Paths.get("EtaX/EtaX.meth2.00.scala"), 6, etaFunction))), // eta-expansion, but lint warning
      testCase(q"val t4b: () => Any = meth2()",   sev => Nil),                                                                            // ditto
      testCase(q"val t4c: () => Any = meth2 _",   sev => List(msg(sev, Paths.get("EtaX/EtaX.meth2.02.scala"), 6, etaFunction))), // ok
      testCase(q"val t4d: () => Any = meth2() _", sev => List(msg(sev, Paths.get("EtaX/EtaX.meth2.03.scala"), 6, etaFunction))), // ok
    ).reduce(_ ++ _)
  }

  object boom extends MkInMemoryTestFile {
    val path         = Paths.get("EtaX/EtaX.boom.lines.scala")
    val outerDefn    = q"class A { def boom(): Unit = () }"
    val testStat     = q"new A().boom // ?/?/err: apply, ()-insertion"
    val path0        = Paths.get("EtaX/EtaX.boom.00.scala")

    val msgs2                = List(warn(     path0, 6, autoApp2("boom")))
    def msgs3(sev: Severity) = List( msg(sev, path0, 6, parensCall3("boom")))

    val expectedMsgs = List(msgs2, msgs2, msgs2, msgs3(Warn), msgs3(Error), msgs3(Error), msgs3(Error))
    val contents     = TestContents(List(List(outerDefn)), None, Nil, List(List(testStat)), expectedMsgs)
  }
}
