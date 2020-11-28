package shisa
package testdata

import scala.Function.const

import scala.meta._

object EtaX {
  def tests: List[TestFile] = boom :: meth2.testFile :: cloneEta :: List(methF0, prop, meth1, meth).map(_.testFile)

  import EtaXErrors._

  object meth extends MkInMemoryTestFile {
    val name = "EtaX.meth"
    val Sam0S = q"                     trait Sam0S { def apply(): Any }"
    val Sam0J = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
    val defns = List(Sam0S, Sam0J, q"def meth() = $ns")
    val stats = List(
      q"val t3a: () => Any = meth                   // eta-expansion, but lint warning",
      q"val t3Sam0S: Sam0S = meth                   // -Xlint:eta-zero + -Xlint:eta-sam",
      q"val t3Sam0J: Sam0J = meth                   // -Xlint:eta-zero",
      q"val t3b: Any       = { val t = meth   ; t } // apply, ()-insertion",
      q"val t3c: () => Any = meth _                 // ok",
      q"val t3d: () => Any = { val t = meth _ ; t } // ok",
      q"val t3e: Any       = meth _                 // ok",
      q"val t3f: Any       = meth() _               // error: _ must follow method",
    )

    val msgs2 = List(
       err(typeMismatch2("String", "p01.Test.Sam0S")),
       err(typeMismatch2("String", "p02.Test.Sam0J")),
      warn(autoApp2("meth")),
       err(mustFollow("String")),
    )
    def msgs3Pair(wore: WorE, exp: String) = List(
      msg(wore.toSev, autoApp3("meth")),
    ) ::: (if (wore == E) Nil else List(
      err(typeMismatch3("String", exp)),
    ))
    def msgs30I(wore: WorE) = {
      msgs3Pair(wore,       "p01.Test.Sam0S") :::
      msgs3Pair(wore,       "p02.Test.Sam0J") ::: List(
      msg(      wore.toSev, autoApp3("meth")),
      msg(      wore.toSev, onlyFuncs("String")),
    )
    }
    def msgs31I(wore: WorE) = List(
      err(            autoApp3("meth")),
      err(            autoApp3("meth")),
      err(            autoApp3("meth")),
      msg(wore.toSev, etaFunction),
      msg(wore.toSev, etaFunction),
      msg(wore.toSev, etaFunction),
      msg(wore.toSev, onlyFuncs("String")),
    )

    val msgs     = List(msgs2, msgs2, msgs30I(W), msgs30I(E), msgs31I(W), msgs31I(E))
    val contents = TestContents(defns, stats.map(List(_)), msgs)
  }

  object meth1 extends MkInMemoryTestFile {
    val name  = "EtaX.meth1"
    val Sam1S = q"                     trait Sam1S { def apply(x: Any): Any }"
    val Sam1J = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"
    val defns = List(Sam1S, Sam1J, q"def meth1(x: Any) = $ns")
    val stats = List(
      q"val t5a: Any => Any = meth1                   // ok",
      q"val t5b: Sam1S      = meth1                   // ok, but warning",
      q"val t5c: Sam1J      = meth1                   // ok",
      q"val t5d: Any => Any = { val t = meth1   ; t } // error in 2.13, eta-expansion in 3.0",
      q"val t5e: Any => Any = { val t = meth1 _ ; t } // ok",
    )
    val msgs2                  = List( err(missingArgs("meth1", "Test")))
    val msgs3                  = List(warn(stillEta("meth1", "p01.Test.Sam1S")))
    def msgs31I(sev: Severity) = List(warn(stillEta("meth1", "p01.Test.Sam1S")), msg(sev, etaFunction2))

    val msgs     = List(msgs2, Nil, msgs3, msgs3, msgs31I(Warn), msgs31I(Error))
    val contents = TestContents(defns, stats.map(List(_)), msgs)
  }

  object prop extends MkInMemoryTestFile {
    val name  = "EtaX.prop"
    val defns = List(q"def prop = $ns")
    val stats = List(
      q"val t2a: () => Any = prop                   // error: no eta-expansion of nullary methods",
      q"val t2b: Any       = { val t = prop   ; t } // ok: apply",
      q"val t2c: () => Any = prop()                 // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument",
      q"val t2d: () => Any = prop _                 // ok",
      q"val t2e: () => Any = { val t = prop _ ; t } // ?/ok",
      q"val t2f: Any       = prop _                 // ok",
      q"val t2g: Any       = prop() _               // error: not enough arguments for method apply",
    )

    def msgs2(wore: WorE)  = List(
      err(            missingArg2("apply: (i: Int): Char", "StringOps", "i")),
      err(            missingArg2("apply: (i: Int): Char", "StringOps", "i")),
      err(            typeMismatch2("String", "() => Any")),
      msg(wore.toSev, methodsWithoutParams(wore)),
      msg(wore.toSev, methodsWithoutParams(wore)),
      msg(wore.toSev, methodsWithoutParams(wore)),
    )
    def msgs30(wore: WorE) = List(
      err(            typeMismatch3("String", "() => Any")),
      err(            missingArg3("apply: (i: Int): Char", "i")),
      msg(wore.toSev, onlyFuncs("String")),
      msg(wore.toSev, onlyFuncs("String")),
      msg(wore.toSev, onlyFuncs("<error unspecified error>")),
      msg(wore.toSev, onlyFuncs("String")),
    ) ::: (if (wore == E) List(
      msg(wore.toSev, typeMismatch3("(t : String)", "() => Any")),
    ) else List(
      err(            missingArg3("apply: (i: Int): Char", "i")),
    ))
    def msgs31(wore: WorE) = List(
      err(            typeMismatch3("String", "() => Any")),
      err(            missingArg3("apply: (i: Int): Char", "i")),
    ) ::: (if (wore == E) Nil else List(
      err(            typeMismatch3("String", "() => Any")),
      err(            missingArg3("apply: (i: Int): Char", "i")),
    )) ::: List(
      msg(wore.toSev, onlyFuncs("<error unspecified error>")),
      msg(wore.toSev, onlyFuncs("String")),
      msg(wore.toSev, onlyFuncs("String")),
      msg(wore.toSev, onlyFuncs("String")),
      err(            typeMismatch3("(t : String)", "() => Any")),
    )

    val contents = TestContents(defns, stats.map(List(_)), multi4(msgs2, msgs30, msgs31))
  }

  object methF0 extends MkInMemoryTestFile {
    val name      = "EtaX.methF0"
    val defns     = List(q"def methF0() = () => $ns")
    val msgs2_1   = warn(   autoApp2("methF0"))
    val msgs30_1  =  msg(_, autoApp3("methF0"))
    val msgs31_1  =  err(   autoApp3("methF0"))
    val msgs31_2  =  msg(_, etaFunction)
    val msgs31_3  =  msg(_, etaFunction)
    val msgs2_4   =  err(   mustFollow("() => String"))
    val msgs30_4  =  msg(_,  onlyFuncs("() => String"))
    val msgs31_4  =  msg(_,  onlyFuncs("() => String"))
    val testCase0 = testCase(q"val t1a: () => Any = methF0",                Nil,           noMsgs,         noMsgs)                // ok, eta-expansion
    val testCase1 = testCase(q"val t1b: () => Any = { val t = methF0; t }", List(msgs2_1), msgs(msgs30_1), msgs(const(msgs31_1))) // `()`-insert b/c no expected type
    val testCase2 = testCase(q"val t1c: () => Any = methF0 _",              Nil,           noMsgs,         msgs(msgs31_2))        // ok, explicit eta-expansion requested
    val testCase3 = testCase(q"val t1d: Any       = methF0 _",              Nil,           noMsgs,         msgs(msgs31_3))        // ok, explicit eta-expansion requested
    val testCase4 = testCase(q"val t1e: Any       = methF0() _",            List(msgs2_4), msgs(msgs30_4), msgs(msgs31_4))        // error: _ must follow method
    val contents  = List(testCase0, testCase1, testCase2, testCase3, testCase4).reduce(_ ++ _)

    def testCase(stat: Stat, msgs2: List[Msg], msgs30: Severity => List[Msg], msgs31: Severity => List[Msg]) = {
      TestContents(defns, List(List(stat)), multi4(const(msgs2), wore => msgs30(wore.toSev), wore => msgs31(wore.toSev)))
    }
  }

  val cloneEta = {
    val stat = q"val ys = { val t = scala.collection.mutable.Map(1 -> 'a'); t.clone }"
    TestFile("EtaX.clone", TestContents(Nil, List(List(stat)), List(Nil, Nil, Nil, Nil, Nil, Nil)))
  }

  object meth2 extends MkInMemoryTestFile {
    val name  = "EtaX.meth2"
    val defns = List(q"def meth2()() = $ns")
    val tc0   = testCase(q"val t4a: () => Any = meth2",     msgs(msg(_, etaFunction))) // eta-expansion, but lint warning
    val tc1   = testCase(q"val t4b: () => Any = meth2()",   noMsgs)                       // ditto
    val tc2   = testCase(q"val t4c: () => Any = meth2 _",   msgs(msg(_, etaFunction))) // ok
    val tc3   = testCase(q"val t4d: () => Any = meth2() _", msgs(msg(_, etaFunction))) // ok
    val contents = List(tc0, tc1, tc2, tc3).reduce(_ ++ _)

    def testCase(stat: Stat, msgs: Severity => List[Msg]) = {
      TestContents(defns, List(List(stat)), multi4(_ => Nil, _ => Nil, wore => msgs(wore.toSev)))
    }
  }

  val boom = {
    val defns = List(q"class A { def boom(): Unit = () }")
    val stat  = q"new A().boom // ?/?/err: apply, ()-insertion"
    val msgss = multi3 {
      case (S2, _)    => List(warn(           autoApp2("boom")))
      case (S3, wore) => List(msg(wore.toSev, autoApp3("boom")))
    }
    TestFile("EtaX.boom", TestContents(defns, List(List(stat)), msgss))
  }
}

object EtaXErrors {
  def etaFunction  = "The syntax `<function> _` is no longer supported;\nyou can use `(() => <function>())` instead"
  def etaFunction2 = "The syntax `<function> _` is no longer supported;\nyou can simply leave out the trailing ` _`"

  def typeMismatch2(obt: String, exp: String) = s"type mismatch;\n found   : $obt\n required: $exp"
  def typeMismatch3(obt: String, exp: String) = s"Found:    $obt\nRequired: $exp"

  def missingArgs(meth: String, cls: String) =
    s"""missing argument list for method $meth in object $cls
       |Unapplied methods are only converted to functions when a function type is expected.
       |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin

  def stillEta(meth: String, traitName: String) =
    s"method $meth is eta-expanded even though $traitName does not have the @FunctionalInterface annotation."

  def mustFollow(tpe: String) = s"_ must follow method; cannot follow $tpe"
  def onlyFuncs(tpe: String)  = s"Only function types can be followed by _ but the current expression has type $tpe"

  def methodsWithoutParams(wore: WorE) = wore match {
    case W => methodsWithoutParamsW
    case E => methodsWithoutParamsNew
  }

  def methodsWithoutParamsW   =
    "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, write a function literal `() => m` instead"
  def methodsWithoutParamsNew =
    "Methods without a parameter list and by-name params can not be converted to functions as `m _`, write a function literal `() => m` instead"

  def missingArg2(meth: String, cls: String, param: String) = s"not enough arguments for method $meth in class $cls.\nUnspecified value parameter $param."
  def missingArg3(meth: String, param: String)              = s"missing argument for parameter $param of method $meth"
}
