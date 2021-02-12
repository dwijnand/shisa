package shisa

import scala.meta._, contrib._

object EtaX {
  def tests: List[TestFile] = List(boom,
    //meth2T,
    meth21, meth22, meth23, meth24,
    cloneEta,
    //methF0T,
    methF0_1, methF0_2, methF0_3, methF0_4, methF0_5,
    prop1, prop2, prop3, prop4, prop5, prop6, prop7,
    meth1T, methSam1S, methSam1J,
    methT,
    //meth01, meth02, meth03, meth04, meth05, meth06,
    methSam0S, methSam0J,
  )

  val ns     = Lit.String("")
  val meth   = q"def meth()        = $ns      "
  val meth1  = q"def meth1(x: Any) = $ns      "
  val methF0 = q"def methF0()      = () => $ns"
  val meth2  = q"def meth2()()     = $ns      "
  val Sam0S  = q"                     trait Sam0S { def apply(): Any }"
  val Sam0J  = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
  val Sam1S  = q"                     trait Sam1S { def apply(x: Any): Any }"
  val Sam1J  = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"

  def msgsFor31(f: Sev => Msg) = multi3(_ => Nil, _ => Nil, sev => List(f(sev)))

  val msgs_fns      = multi3(_ => List(err(mustFollow("String"))), sev => List(Msg(sev, onlyFuncs("String"))), sev => List(Msg(sev, onlyFuncs("String"))))
  val msgs_missArgs = multi2 { case (S2, W) => List(err(missingArgs("meth1", "Test"))) case _        => Nil                                 }

  val meth01 = mkTest(meth,  q"val t3a: () => Any  = meth                   ", noMsgs)
  val meth02 = mkTest(meth,  q"val t3b: Any        = { val t = meth; t }    ", autoApp(q"meth"))
  val meth03 = mkTest(meth,  q"val t3c: () => Any  = meth _                 ", msgsFor31(Msg(_, etaFunction)))
  val meth04 = mkTest(meth,  q"val t3d: () => Any  = { val t = meth _ ; t } ", msgsFor31(Msg(_, etaFunction)))
  val meth05 = mkTest(meth,  q"val t3e: Any        = meth _                 ", msgsFor31(Msg(_, etaFunction)))
  val meth06 = mkTest(meth,  q"val t3f: Any        = meth() _               ", msgs_fns)
  val meth11 = mkTest(meth1, q"val t5a: Any => Any = meth1                  ", noMsgs)
  val meth12 = mkTest(meth1, q"val t5d: Any => Any = { val t = meth1   ; t }", msgs_missArgs)
  val meth13 = mkTest(meth1, q"val t5e: Any => Any = { val t = meth1 _ ; t }", msgsFor31(Msg(_, etaFunction2)))

  val methT  = mkFile("EtaX.meth",  List(meth01, meth02, meth03, meth04, meth05, meth06))
  val meth1T = mkFile("EtaX.meth1", List(meth11, meth12, meth13))

  def msgs_sam0(pt: String) = multi2 {
    case (S2,   _) => List(err(typeMismatch2("String", pt)))
    case (S3, sev) => mkAutoApp(q"meth")(S3, sev) match {
      case autoAppMsg @ Msg(E, _) => List(autoAppMsg)
      case autoAppMsg             => List(autoAppMsg, err(typeMismatch3("String", pt)))
    }
  }
  def mkSam0(stat: Stat, samDefn: Defn.Trait)                        = TestContents(List(samDefn, meth ), List(stat), msgs_sam0(s"Test.${samDefn.name}"))
  def mkSam1(stat: Stat, samDefn: Defn.Trait, msgs: List[List[Msg]]) = TestContents(List(samDefn, meth1), List(stat), msgs)
  val msg3SamEta = List(warn(stillEta("meth1", "Test.Sam1S")))

  val methSam0S = TestFile("EtaX.methSam0S", mkSam0(q"val t3Sam0S: Sam0S = meth", Sam0S))
  val methSam0J = TestFile("EtaX.methSam0J", mkSam0(q"val t3Sam0J: Sam0J = meth", Sam0J))
  val methSam1S = TestFile("EtaX.methSam1S", mkSam1(q"val t5b: Sam1S     = meth1", Sam1S, multi3(_ => Nil, _ => msg3SamEta, _ => msg3SamEta)))
  val methSam1J = TestFile("EtaX.methSam1J", mkSam1(q"val t5c: Sam1J     = meth1", Sam1J, noMsgs))

  val prop  = q"def prop = ${Lit.String("")}"
  val defns = List(prop)

  def msgsD(m2: Msg, m3: Msg)               = multi3(  _ => List(m2),        _ => List(m3),        _ => List(m3))
  def msgsE(m2: Sev => Msg, m3: Sev => Msg) = multi3(sev => List(m2(sev)), sev => List(m3(sev)), sev => List(m3(sev)))

  val msgsProp1 = msgsD(err(typeMismatch2("String", "() => Any")), err(typeMismatch3("String", "() => Any")))
  val msgsProp3 = msgsD(err(missingArg2("apply: (i: Int): Char", "StringOps", "i")), err(missingArg3("apply: (i: Int): Char", "i")))

  val msgsProp4 = multi3(
    sev => List(Msg(sev, methodsWithoutParams(sev))),
    sev => List(Msg(sev, onlyFuncs("String"))),
    {
      case W => List(Msg(W, onlyFuncs("String")), err(typeMismatch3("String", "() => Any")))
      case E => List(Msg(E, onlyFuncs("String")))
    }
  )

  val msgsProp5 = multi3(
    sev => List(Msg(sev, methodsWithoutParams(sev))),
    {
      case W => List(Msg(W, onlyFuncs("String")))
      case E => List(Msg(E, onlyFuncs("String")), err(typeMismatch3("(t : String)", "() => Any")))
    },
    sev => List(Msg(sev, onlyFuncs("String")), err(typeMismatch3("(t : String)", "() => Any"))),
  )

  val msgsProp6 = multi3(
    sev => List(Msg(sev, methodsWithoutParams(sev))),
    sev => List(Msg(sev, onlyFuncs("String"))),
    sev => List(Msg(sev, onlyFuncs("String"))),
  )

  val msgsProp7 = multi3(
    _ => List(err(missingArg2("apply: (i: Int): Char", "StringOps", "i"))),
    {
      case W => List(Msg(W, onlyFuncs("<error unspecified error>")), err(missingArg3("apply: (i: Int): Char", "i")))
      case E => List(Msg(E, onlyFuncs("<error unspecified error>")))
    },
    {
      case W => List(Msg(W, onlyFuncs("<error unspecified error>")), err(missingArg3("apply: (i: Int): Char", "i")))
      case E => List(Msg(E, onlyFuncs("<error unspecified error>")))
    },
  )

  val msgEtaF     = multi3(_ => Nil, _ => Nil, msgs(Msg(_, etaFunction)))

  val msgMethF0_2 = multi3(_ => List(Msg(W, autoApp2("methF0"))),      sev => List(Msg(sev, autoApp3("methF0"))),        sev => List(Msg(  E, autoApp3("methF0"))))
  val msgMethF0_5 = multi3(_ => List(err(mustFollow("() => String"))), sev => List(Msg(sev, onlyFuncs("() => String"))), sev => List(Msg(sev, onlyFuncs("() => String"))))

  val prop1    = TestFile("EtaX.prop1",    mkTest(prop,   q"val t2a: () => Any = prop                  ", msgsProp1))
  val prop2    = TestFile("EtaX.prop2",    mkTest(prop,   q"val t2b: Any       = { val t = prop   ; t }", noMsgs))
  val prop3    = TestFile("EtaX.prop3",    mkTest(prop,   q"val t2c: () => Any = prop()                ", msgsProp3))
  val prop4    = TestFile("EtaX.prop4",    mkTest(prop,   q"val t2d: () => Any = prop _                ", msgsProp4))
  val prop5    = TestFile("EtaX.prop5",    mkTest(prop,   q"val t2e: () => Any = { val t = prop _ ; t }", msgsProp5))
  val prop6    = TestFile("EtaX.prop6",    mkTest(prop,   q"val t2f: Any       = prop _                ", msgsProp6))
  val prop7    = TestFile("EtaX.prop7",    mkTest(prop,   q"val t2g: Any       = prop() _              ", msgsProp7))
  val methF0_1 = TestFile("EtaX.methF0_1", mkTest(methF0, q"val t1a: () => Any = methF0                ", noMsgs))
  val methF0_2 = TestFile("EtaX.methF0_2", mkTest(methF0, q"val t1b: () => Any = { val t = methF0; t } ", msgMethF0_2))
  val methF0_3 = TestFile("EtaX.methF0_3", mkTest(methF0, q"val t1c: () => Any = methF0 _              ", msgEtaF))
  val methF0_4 = TestFile("EtaX.methF0_4", mkTest(methF0, q"val t1d: Any       = methF0 _              ", msgEtaF))
  val methF0_5 = TestFile("EtaX.methF0_5", mkTest(methF0, q"val t1e: Any       = methF0() _            ", msgMethF0_5))
  val meth21   = TestFile("EtaX.meth21",   mkTest(meth2,  q"val t4a: () => Any = meth2                 ", msgEtaF))
  val meth22   = TestFile("EtaX.meth22",   mkTest(meth2,  q"val t4b: () => Any = meth2()               ", noMsgs))
  val meth23   = TestFile("EtaX.meth23",   mkTest(meth2,  q"val t4c: () => Any = meth2 _               ", msgEtaF))
  val meth24   = TestFile("EtaX.meth24",   mkTest(meth2,  q"val t4d: () => Any = meth2() _             ", msgEtaF))

  val boom     = TestFile("EtaX.boom",     mkTest(q"class A { def boom(): Unit = () }", q"new A().boom", autoApp(q"boom")))
  val cloneEta = TestFile("EtaX.clone",    TestContents(Nil, List(q"val ys = { val t = scala.collection.mutable.Map(1 -> 'a'); t.clone }"), noMsgs))

//val methF0T  = mkFile("EtaX.methF0", List(methF0_1, methF0_2, methF0_3, methF0_4, methF0_5))
//val meth2T   = mkFile("EtaX.meth2",  List(meth21, meth23, meth23, meth24))

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
