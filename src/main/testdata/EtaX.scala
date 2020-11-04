package shisa
package testdata

import java.nio.file._

import scala.Function.const

import scala.meta._

import Severity.{ Warn, Error }

trait MkInMemoryTestLinesFile extends MkInMemoryTestFile {
  def pathN(n: Int): Path = {
    val name = path.getFileName.toString.stripSuffix(".lines.scala")
    val idx  = if (n < 10) s"0$n" else s"$n"
    path.resolveSibling(s"$name.$idx.scala")
  }
}

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

  object meth extends MkInMemoryTestLinesFile {
    val path          = Paths.get("EtaX/EtaX.meth.lines.scala")
    def Sam0S         = q"                     trait Sam0S { def apply(): Any }"
    def Sam0J         = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
    def outerDefns    = List(Sam0S, Sam0J)
    def baseClass     = q"""class TestBase { def meth() = "" }"""
    def testStats     = List(
      q"val t3a: () => Any = meth                   // eta-expansion, but lint warning",
      q"val t3Sam0S: Sam0S = meth                   // -Xlint:eta-zero + -Xlint:eta-sam",
      q"val t3Sam0J: Sam0J = meth                   // -Xlint:eta-zero",
      q"val t3b: Any       = { val t = meth   ; t } // apply, ()-insertion",
      q"val t3c: () => Any = meth _                 // ok",
      q"val t3d: () => Any = { val t = meth _ ; t } // ok",
      q"val t3e: Any       = meth _                 // ok",
      q"val t3f: Any       = meth() _               // error: _ must follow method",
    )

    val msgs2         = List(
       err(Paths.get("EtaX/EtaX.meth.01.scala"), 5, typeMismatch2("String", "p01.Sam0S")),
       err(Paths.get("EtaX/EtaX.meth.02.scala"), 5, typeMismatch2("String", "p02.Sam0J")),
      warn(Paths.get("EtaX/EtaX.meth.03.scala"), 7, autoApp2("meth")),
       err(Paths.get("EtaX/EtaX.meth.07.scala"), 5, mustFollow("String")),
    )
    def msgs3Pair(sev: Severity, path: Path, lineNo: Int, exp: String) = List(
      msg(sev, path, lineNo, parensCall3("meth")),
    ) ::: (if (sev == Error) Nil else List(
      err(     path, lineNo, typeMismatch3("String", exp)),
    ))
    def msgs3I(sev: Severity) =
      msgs3Pair(sev, Paths.get("EtaX/EtaX.meth.01.scala"), 5, "p01.Sam0S") :::
      msgs3Pair(sev, Paths.get("EtaX/EtaX.meth.02.scala"), 5, "p02.Sam0J") ::: List(
      msg(      sev, Paths.get("EtaX/EtaX.meth.03.scala"), 7, mustParens("meth")),
      msg(      sev, Paths.get("EtaX/EtaX.meth.07.scala"), 5, onlyFuncs("String")),
    )
    def msgs31I(sev: Severity) = List(
      err(     Paths.get("EtaX/EtaX.meth.01.scala"), 5, parensCall3("meth")),
      err(     Paths.get("EtaX/EtaX.meth.02.scala"), 5, parensCall3("meth")),
      err(     Paths.get("EtaX/EtaX.meth.03.scala"), 7, parensCall3("meth")),
      msg(sev, Paths.get("EtaX/EtaX.meth.04.scala"), 5, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.meth.05.scala"), 7, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.meth.06.scala"), 5, etaFunction),
      msg(sev, Paths.get("EtaX/EtaX.meth.07.scala"), 5, onlyFuncs("String")),
    )

    val expectedMsgs  = List(msgs2, msgs2, msgs2, msgs3I(Warn), msgs3I(Error), msgs31I(Warn), msgs31I(Error))
    val contents      = TestContents(outerDefns, Some(baseClass), Nil, testStats, expectedMsgs)
  }

  object meth1 extends MkInMemoryTestLinesFile {
    val path          = Paths.get("EtaX/EtaX.meth1.lines.scala")
    def Sam1S         = q"                     trait Sam1S { def apply(x: Any): Any }"
    def Sam1J         = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"
    def outerDefns    = List(Sam1S, Sam1J)
    def baseClass     = q"""class TestBase { def meth1(x: Any) = "" }"""
    def testStats     = List(
      q"val t5a: Any => Any = meth1                   // ok",
      q"val t5b: Sam1S      = meth1                   // ok, but warning",
      q"val t5c: Sam1J      = meth1                   // ok",
      q"val t5d: Any => Any = { val t = meth1   ; t } // error in 2.13, eta-expansion in 3.0",
      q"val t5e: Any => Any = { val t = meth1 _ ; t } // ok",
    )

    val msgs2                  = List( err(Paths.get("EtaX/EtaX.meth1.03.scala"), 7, missingArgs("meth1", "TestBase")))
    val msgs3                  = List(warn(Paths.get("EtaX/EtaX.meth1.01.scala"), 5, stillEta("meth1", "p01.Sam1S")))
    def msgs31I(sev: Severity) = msgs3 ::: List(msg(sev, Paths.get("EtaX/EtaX.meth1.04.scala"), 7, etaFunction2))

    val expectedMsgs = List(msgs2, msgs2, Nil, msgs3, msgs3, msgs31I(Warn), msgs31I(Error))
    val contents     = TestContents(outerDefns, Some(baseClass), Nil, testStats, expectedMsgs)
  }

  object prop extends MkInMemoryTestLinesFile {
    val path         = Paths.get("EtaX/EtaX.prop.lines.scala")
    def baseClass    = q"""class TestBase { def prop = "" }"""

    def testStats    = List(
      q"val t2a: () => Any = prop                   // error: no eta-expansion of nullary methods",
      q"val t2b: Any       = { val t = prop   ; t } // ok: apply",
      q"val t2c: () => Any = prop()                 // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument",
      q"val t2d: () => Any = prop _                 // ok",
      q"val t2e: () => Any = { val t = prop _ ; t } // ?/ok",
      q"val t2f: Any       = prop _                 // ok",
      q"val t2g: Any       = prop() _               // error: not enough arguments for method apply",
    )

    def msgs2(sev: Severity)  = List(
      err(     pathN(0), 3, typeMismatch2("String", "() => Any")),
      err(     pathN(2), 3, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
      msg(sev, pathN(3), 3, if (sev == Error) methodsWithoutParamsNew else methodsWithoutParams),
      msg(sev, pathN(4), 5, if (sev == Error) methodsWithoutParamsNew else methodsWithoutParams),
      msg(sev, pathN(5), 3, if (sev == Error) methodsWithoutParamsNew else methodsWithoutParams),
      err(     pathN(6), 3, notEnoughArgs("apply: (i: Int): Char", "StringOps", "i")),
    )
    def msgs3(sev: Severity)  = List(
      err(     pathN(0), 3, typeMismatch3("String", "() => Any")),
      err(     pathN(2), 3, missingArgForParam("apply: (i: Int): Char", "i")),
      msg(sev, pathN(3), 3, onlyFuncs("String")),
      msg(sev, pathN(4), 5, onlyFuncs("String")),
    ) ::: (if (sev != Error) Nil else List(
      msg(sev, pathN(4), 6, typeMismatch3("(t : String)", "() => Any")),
    )) ::: List(
      msg(sev, pathN(5), 3, onlyFuncs("String")),
      msg(sev, pathN(6), 3, onlyFuncs("<error unspecified error>")),
    ) ::: (if (sev == Error) Nil else List(
      err(     pathN(6), 3, missingArgForParam("apply: (i: Int): Char", "i"))
    ))
    def msgs31(sev: Severity) = List(
      err(     pathN(0), 3, typeMismatch3("String", "() => Any")),
      err(     pathN(2), 3, missingArgForParam("apply: (i: Int): Char", "i")),
      msg(sev, pathN(3), 3, onlyFuncs("String")),
    ) ::: (if (sev == Error) Nil else List(
      err(     pathN(3), 3, typeMismatch3("String", "() => Any")),
    )) ::: List(
      msg(sev, pathN(4), 5, onlyFuncs("String")),
      err(     pathN(4), 6, typeMismatch3("(t : String)", "() => Any")),
      msg(sev, pathN(5), 3, onlyFuncs("String")),
      msg(sev, pathN(6), 3, onlyFuncs("<error unspecified error>")),
    ) ::: (if (sev == Error) Nil else List(
      err(     pathN(6), 3, missingArgForParam("apply: (i: Int): Char", "i")),
    ))

    val expectedMsgs = List(msgs2(Warn), msgs2(Warn), msgs2(Error), msgs3(Warn), msgs3(Error), msgs31(Warn), msgs31(Error))
    val contents     = TestContents(Nil, Some(baseClass), Nil, testStats, expectedMsgs)

    def testCase(stat: Stat, msgs2: Severity => List[Msg], msgs30: Severity => List[Msg], msgs31: Severity => List[Msg]) = {
      val expectedMsgs = List(msgs2(Warn), msgs2(Warn), msgs2(Error), msgs30(Warn), msgs30(Error), msgs31(Warn), msgs31(Error))
      TestContents(Nil, Some(baseClass), Nil, List(stat), expectedMsgs)
    }
  }

  object methF0 extends MkInMemoryTestLinesFile {
    val path      = Paths.get("EtaX/EtaX.methF0.lines.scala")
    val baseClass = q"""class TestBase { def methF0() = () => "" }"""

    val msgs2_1   = warn(   pathN(1), 5, autoApp2("methF0"))
    val msgs30_1  =  msg(_, pathN(1), 5, parensCall3("methF0"))
    val msgs31_1  =  err(   pathN(1), 5, parensCall3("methF0"))
    val msgs31_2  =  msg(_, pathN(2), 3, etaFunction)
    val msgs31_3  =  msg(_, pathN(3), 3, etaFunction)
    val msgs2_4   =  err(   pathN(4), 3, mustFollow("() => String"))
    val msgs30_4  =  msg(_, pathN(4), 3,  onlyFuncs("() => String"))
    val msgs31_4  =  msg(_, pathN(4), 3,  onlyFuncs("() => String"))

    val testCase0 = testCase(q"val t1a: () => Any = methF0",                Nil,           noMsgs,         noMsgs)                // ok, eta-expansion
    val testCase1 = testCase(q"val t1b: () => Any = { val t = methF0; t }", List(msgs2_1), msgs(msgs30_1), msgs(const(msgs31_1))) // `()`-insert b/c no expected type
    val testCase2 = testCase(q"val t1c: () => Any = methF0 _",              Nil,           noMsgs,         msgs(msgs31_2))        // ok, explicit eta-expansion requested
    val testCase3 = testCase(q"val t1d: Any       = methF0 _",              Nil,           noMsgs,         msgs(msgs31_3))        // ok, explicit eta-expansion requested
    val testCase4 = testCase(q"val t1e: Any       = methF0() _",            List(msgs2_4), msgs(msgs30_4), msgs(msgs31_4))        // error: _ must follow method
    val contents  = List(testCase0, testCase1, testCase2, testCase3, testCase4).reduce(_ ++ _)

    def testCase(stat: Stat, msgs2: List[Msg], msgs30: Severity => List[Msg], msgs31: Severity => List[Msg]) = {
      val expectedMsgs = List(msgs2, msgs2, msgs2, msgs30(Warn), msgs30(Error), msgs31(Warn), msgs31(Error))
      TestContents(Nil, Some(baseClass), Nil, List(stat), expectedMsgs)
    }
  }

  object cloneEta extends MkInMemoryTestUnitFile {
    val path         = Paths.get("EtaX/EtaX.clone.scala")
    def testStat     = q"val ys = { val t = scala.collection.mutable.Map(1 -> 'a'); t.clone }"
    val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, Nil, Nil)
    val contents     = TestContents(Nil, None, Nil, List(testStat), expectedMsgs)
  }

  object meth2 extends MkInMemoryTestLinesFile {
    val path      = Paths.get("EtaX/EtaX.meth2.lines.scala")
    val baseClass = q"""class TestBase { def meth2()() = "" }"""
    val testCase0 = testCase(q"val t4a: () => Any = meth2",     msgs(msg(_, pathN(0), 3, etaFunction))) // eta-expansion, but lint warning
    val testCase1 = testCase(q"val t4b: () => Any = meth2()",   noMsgs)                                 // ditto
    val testCase2 = testCase(q"val t4c: () => Any = meth2 _",   msgs(msg(_, pathN(2), 3, etaFunction))) // ok
    val testCase3 = testCase(q"val t4d: () => Any = meth2() _", msgs(msg(_, pathN(3), 3, etaFunction))) // ok
    val contents  = List(testCase0, testCase1, testCase2, testCase3).reduce(_ ++ _)

    def testCase(stat: Stat, msgs: Severity => List[Msg]) = {
      val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, msgs(Warn), msgs(Error))
      TestContents(Nil, Some(baseClass), Nil, List(stat), expectedMsgs)
    }
  }

  object boom extends MkInMemoryTestUnitFile {
    val path         = Paths.get("EtaX/EtaX.boom.scala")
    val outerDefn    = q"class A { def boom(): Unit = () }"
    val testStat     = q"new A().boom // ?/?/err: apply, ()-insertion"
    val msgs2        = List(warn(         2, autoApp2("boom")))
    val msgs3        = msgs( msg(_, path, 2, parensCall3("boom")))
    val expectedMsgs = List(msgs2, msgs2, msgs2, msgs3(Warn), msgs3(Error), msgs3(Error), msgs3(Error))
    val contents     = TestContents(List(outerDefn), None, Nil, List(testStat), expectedMsgs)
  }

  def msgs(mkMsg: Severity => Msg) : Severity => List[Msg] = sev => List(mkMsg(sev))
  def noMsgs                       : Severity => List[Msg] = _   => Nil
}
