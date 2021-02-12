package shisa

import scala.Function.const

import scala.meta._, contrib._

object EtaX {
  def tests: List[TestFile] = List(boom, meth2, cloneEta, methF0, prop, meth1, methT, methSamS, methSamJ)

  val meth        = q"def meth() = ${Lit.String("")}"
  val msgs_31_eta = multi3(_ => Nil, _ => Nil, sev => List(Msg(sev, etaFunction)))
  val meth01      = mkTest(meth, q"val t3a: () => Any = meth                  ", noMsgs)
  val meth04      = mkTest(meth, q"val t3b: Any       = { val t = meth; t }   ", autoApp(q"meth"))
  val meth05      = mkTest(meth, q"val t3c: () => Any = meth _                ", msgs_31_eta)
  val meth06      = mkTest(meth, q"val t3d: () => Any = { val t = meth _ ; t }", msgs_31_eta)
  val meth07      = mkTest(meth, q"val t3e: Any       = meth _                ", msgs_31_eta)
  val meth08      = {
    val msgs3 = (sev: Sev) => List(Msg(sev, onlyFuncs("String")))
    val msgs  = multi3(_ => List(Msg(E, mustFollow("String"))), msgs3, msgs3)
    mkTest(meth, q"val t3f: Any = meth() _", msgs)
  }
  val methT       = mkFile("EtaX.meth", List(meth01, meth04, meth05, meth06, meth07, meth08))

  def mkSam(stat: Stat, samDefn: Defn.Trait) = {
    val pt = s"Test.${samDefn.name}"
    val msgs = multi2 {
      case (S2,   _) => List(err(typeMismatch2("String", pt)))
      case (S3, sev) => mkAutoApp(q"meth")(S3, sev) match {
        case autoAppMsg @ Msg(E, _) => List(autoAppMsg)
        case autoAppMsg             => List(autoAppMsg, err(typeMismatch3("String", pt)))
      }
    }
    TestContents(List(samDefn, meth), List(stat), msgs)
  }

  val Sam0S    = q"                     trait Sam0S { def apply(): Any }"
  val Sam0J    = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
  val methSamS = TestFile("EtaX.methSamS", mkSam(q"val t3Sam0S: Sam0S = meth", Sam0S))
  val methSamJ = TestFile("EtaX.methSamJ", mkSam(q"val t3Sam0J: Sam0J = meth", Sam0J))

  val meth1 = {
    val Sam1S   = q"                     trait Sam1S { def apply(x: Any): Any }"
    val Sam1J   = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"
    val meth1   = q"def meth1(x: Any) = ${Lit.String("")}"
    val defns   = List(Sam1S, Sam1J, meth1)
    val stat1   = q"val t5a: Any => Any = meth1                   // ok"
    val stat2   = q"val t5b: Sam1S      = meth1                   // ok, but warning"
    val stat3   = q"val t5c: Sam1J      = meth1                   // ok"
    val stat4   = q"val t5d: Any => Any = { val t = meth1   ; t } // error in 2.13, eta-expansion in 3.0"
    val stat5   = q"val t5e: Any => Any = { val t = meth1 _ ; t } // ok"
    val stats   = List(stat1, stat2, stat3, stat4, stat5)
    val msg2_4  =  err(missingArgs("meth1", "Test"))
    val msg3_2  = warn(stillEta("meth1", "p01.Test.Sam1S"))
    val msgEta2 = (sev: Sev) => Msg(sev, etaFunction2)
    val msgs2   = List(msg2_4)
    val msgs3   = List(msg3_2)
    val msgs31M = (sev: Sev) => List(msg3_2, msgEta2(sev))
    val msgs    = List(msgs2, Nil, msgs3, msgs3, msgs31M(W), msgs31M(E))
    TestFile("EtaX.meth1", TestContents(defns, stats, msgs))
  }

  val prop = {
    val defns = List(q"def prop = ${Lit.String("")}")
    val stats = List(
      q"val t2a: () => Any = prop                   // error: no eta-expansion of nullary methods",
      q"val t2b: Any       = { val t = prop   ; t } // ok: apply",
      q"val t2c: () => Any = prop()                 // error: bar doesn't take arguments, so expanded to bar.apply(), which misses an argument",
      q"val t2d: () => Any = prop _                 // ok",
      q"val t2e: () => Any = { val t = prop _ ; t } // ?/ok",
      q"val t2f: Any       = prop _                 // ok",
      q"val t2g: Any       = prop() _               // error: not enough arguments for method apply",
    )

    def msgs2(sev: Sev)  = List(
      err(     missingArg2("apply: (i: Int): Char", "StringOps", "i")),
      err(     missingArg2("apply: (i: Int): Char", "StringOps", "i")),
      err(     typeMismatch2("String", "() => Any")),
      Msg(sev, methodsWithoutParams(sev)), Msg(sev, methodsWithoutParams(sev)), Msg(sev, methodsWithoutParams(sev)),
    )
    def msgs30(sev: Sev) = List(
      err(     typeMismatch3("String", "() => Any")),
      err(     missingArg3("apply: (i: Int): Char", "i")),
      Msg(sev, onlyFuncs("String")),
      Msg(sev, onlyFuncs("String")),
      Msg(sev, onlyFuncs("<error unspecified error>")),
      Msg(sev, onlyFuncs("String")),
    ) ::: (
      if (sev == E) List(err(typeMismatch3("(t : String)", "() => Any")))
      else          List(err(missingArg3("apply: (i: Int): Char", "i")))
    )
    def msgs31(sev: Sev) = List(
      err(     typeMismatch3("String", "() => Any")),
      err(       missingArg3("apply: (i: Int): Char", "i")),
    ) ::: (if (sev == E) Nil else List(
      err(     typeMismatch3("String", "() => Any")),
      err(       missingArg3("apply: (i: Int): Char", "i")),
    )) ::: List(
      Msg(sev, onlyFuncs("<error unspecified error>")),
      Msg(sev, onlyFuncs("String")),
      Msg(sev, onlyFuncs("String")),
      Msg(sev, onlyFuncs("String")),
      err(     typeMismatch3("(t : String)", "() => Any")),
    )

    val msgs  = multi3(msgs2, msgs30, msgs31)
    TestFile("EtaX.prop", TestContents(defns, stats, msgs))
  }

  val methF0 = {
    val defns     = List(q"def methF0() = () => ${Lit.String("")}")
    def testCase(stat: Stat, msgs2: List[Msg], msgs30: Sev => List[Msg], msgs31: Sev => List[Msg]) =
      TestContents(defns, List(stat), multi3(const(msgs2), msgs30, msgs31))
    val msgs1_2   = warn(  autoApp2("methF0"))
    val msgs1_30  = Msg(_, autoApp3("methF0"))
    val msgs1_31  = err(   autoApp3("methF0"))
    val msgs4_2   = err(   mustFollow("() => String"))
    val msgs4_30  = Msg(_, onlyFuncs("() => String"))
    val msgs4_31  = Msg(_, onlyFuncs("() => String"))
    val testCase0 = testCase(q"val t1a: () => Any = methF0",                Nil,           mkNoMsgs,       mkNoMsgs)                  // ok, eta-expansion
    val testCase1 = testCase(q"val t1b: () => Any = { val t = methF0; t }", List(msgs1_2), msgs(msgs1_30), msgs(const(msgs1_31)))     // `()`-insert b/c no expected type
    val testCase2 = testCase(q"val t1c: () => Any = methF0 _",              Nil,           mkNoMsgs,       msgs(Msg(_, etaFunction))) // ok, explicit eta-expansion requested
    val testCase3 = testCase(q"val t1d: Any       = methF0 _",              Nil,           mkNoMsgs,       msgs(Msg(_, etaFunction))) // ok, explicit eta-expansion requested
    val testCase4 = testCase(q"val t1e: Any       = methF0() _",            List(msgs4_2), msgs(msgs4_30), msgs(msgs4_31))            // error: _ must follow method
    val contents  = List(testCase0, testCase1, testCase2, testCase3, testCase4).reduce(_ ++ _)
    TestFile("EtaX.methF0", contents)
  }

  val cloneEta = {
    val stat = q"val ys = { val t = scala.collection.mutable.Map(1 -> 'a'); t.clone }"
    TestFile("EtaX.clone", TestContents(Nil, List(stat), noMsgs))
  }

  val meth2 = {
    val defns = List(q"def meth2()() = ${Lit.String("")}")
    def testCase(stat: Stat, msgs: Sev => List[Msg]) =
      TestContents(defns, List(stat), multi3(_ => Nil, _ => Nil, msgs))
    val tc0   = testCase(q"val t4a: () => Any = meth2",     msgs(Msg(_, etaFunction))) // eta-expansion, but lint warning
    val tc1   = testCase(q"val t4b: () => Any = meth2()",   mkNoMsgs)                  // ditto
    val tc2   = testCase(q"val t4c: () => Any = meth2 _",   msgs(Msg(_, etaFunction))) // ok
    val tc3   = testCase(q"val t4d: () => Any = meth2() _", msgs(Msg(_, etaFunction))) // ok
    TestFile("EtaX.meth2", List(tc0, tc1, tc2, tc3).reduce(_ ++ _))
  }

  val boom = {
    val defn = q"class A { def boom(): Unit = () }"
    val stat = q"new A().boom // ?/?/err: apply, ()-insertion"
    TestFile("EtaX.boom", mkTest(defn, stat, autoApp(q"boom")))
  }

  // Errors
  def etaFunction  = "The syntax `<function> _` is no longer supported;\nyou can use `(() => <function>())` instead"
  def etaFunction2 = "The syntax `<function> _` is no longer supported;\nyou can simply leave out the trailing ` _`"

  def typeMismatch2(tp: String, pt: String) = s"type mismatch;\n found   : $tp\n required: $pt"
  def typeMismatch3(tp: String, pt: String) = s"Found:    $tp\nRequired: $pt"

  def missingArgs(meth: String, cls: String) =
    s"""missing argument list for method $meth in object $cls
       |Unapplied methods are only converted to functions when a function type is expected.
       |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin

  def stillEta(meth: String, traitName: String) =
    s"method $meth is eta-expanded even though $traitName does not have the @FunctionalInterface annotation."

  def mustFollow(tpe: String) = s"_ must follow method; cannot follow $tpe"
  def onlyFuncs(tpe: String)  = s"Only function types can be followed by _ but the current expression has type $tpe"

  def methodsWithoutParams(sev: Sev) = sev match {
    case W => methodsWithoutParamsW
    case E => methodsWithoutParamsNew
  }

  def methodsWithoutParamsW   =
    "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, write a function literal `() => m` instead"
  def methodsWithoutParamsNew =
    "Methods without a parameter list and by-name params can not be converted to functions as `m _`, write a function literal `() => m` instead"

  def missingArg2(meth: String, cls: String, param: String) = s"not enough arguments for method $meth in class $cls.\nUnspecified value parameter $param."
  def missingArg3(meth: String, param: String)              = s"missing argument for parameter $param of method $meth"

  def mkNoMsgs(sev: Sev)      = Nil
  def msgs(mkMsg: Sev => Msg) = (sev: Sev) => List(mkMsg(sev))
}
