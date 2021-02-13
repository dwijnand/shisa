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
  val prop   = q"def prop = ${Lit.String("")}"
  val Sam0S  = q"                     trait Sam0S { def apply(): Any }"
  val Sam0J  = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
  val Sam1S  = q"                     trait Sam1S { def apply(x: Any): Any }"
  val Sam1J  = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"

  def onlyFuncs(tp: String)                  = msgsFor3(onlyFuncsText(_, tp))
  def typeMismatches(tp: String, pt: String) = msgsFor2(_ => typeMismatch2(tp, pt)) ++ msgsFor3(_ => typeMismatch3(tp, pt))
  def typeMismatch2(tp: String, pt: String)  = TypeMismatchMsg(E, s"type mismatch;\n found   : $tp\n required: $pt")
  def typeMismatch3(tp: String, pt: String)  = TypeMismatchMsg(E, s"Found:    $tp\nRequired: $pt")
  def mustFollow(tp: String)                 = err(s"_ must follow method; cannot follow $tp")
  def onlyFuncsText(sev: Sev, tp: String)    = Msg(sev, s"Only function types can be followed by _ but the current expression has type $tp")

  val prop1Msgs = typeMismatches("String", "() => Any")
  val prop3Msgs =   missingArgs("apply: (i: Int): Char", "StringOps", "i")

  val prop4Msgs = noEtaNullary ++ Msgs(Nil, Nil,
    List(onlyFuncsText(W, "String")), // type mismatch suppressed?
    List(onlyFuncsText(E, "String")), // kills
    onlyFuncsText(W, "String") :+ typeMismatch3("String", "() => Any"),
    onlyFuncsText(E, "String") :+ typeMismatch3("String", "() => Any"),
  )

  val prop5Msgs = noEtaNullary ++ Msgs(Nil, Nil,
    List(onlyFuncsText(W, "String")), // type mismatch suppressed?
    List(onlyFuncsText(E, "String"), typeMismatch3("(t : String)", "() => Any")),
    List(onlyFuncsText(W, "String"), typeMismatch3("(t : String)", "() => Any")),
    List(onlyFuncsText(E, "String"), typeMismatch3("(t : String)", "() => Any")),
  )

  val prop6Msgs = noEtaNullary ++ onlyFuncs("String")
  val prop7Msgs = onlyFuncs("<error unspecified error>") ++ missingArgs("apply: (i: Int): Char", "StringOps", "i")

  val meth01    =                  mkTest(meth,   q"val t3a: () => Any  = meth                   ", noMsgs)
  val meth02    =                  mkTest(meth,   q"val t3b: Any        = { val t = meth; t }    ", autoApp(q"meth"))
  val meth03    =                  mkTest(meth,   q"val t3c: () => Any  = meth _                 ", msgsForEtaX())
  val meth04    =                  mkTest(meth,   q"val t3d: () => Any  = { val t = meth _ ; t } ", msgsForEtaX())
  val meth05    =                  mkTest(meth,   q"val t3e: Any        = meth _                 ", msgsForEtaX())
  val meth06    =                  mkTest(meth,   q"val t3f: Any        = meth() _               ", msgsFor2(_ => mustFollow("String")) ++ onlyFuncs("String"))

  val meth11    =                  mkTest(meth1,  q"val t5a: Any => Any = meth1                  ", noMsgs)
  val meth12    =                  mkTest(meth1,  q"val t5d: Any => Any = { val t = meth1   ; t }", noArgsList("meth1", "object Test"))
  val meth13    =                  mkTest(meth1,  q"val t5e: Any => Any = { val t = meth1 _ ; t }", msgsForEtaX(simple = true))
  val methSam0S = mkF("methSam0S", mkSam0(        q"val t3Sam0S: Sam0S = meth                    ", Sam0S, sam0Msgs("Test.Sam0S")))
  val methSam0J = mkF("methSam0J", mkSam0(        q"val t3Sam0J: Sam0J = meth                    ", Sam0J, sam0Msgs("Test.Sam0J")))
  val methSam1S = mkF("methSam1S", mkSam1(        q"val t5b: Sam1S     = meth1                   ", Sam1S, stillEta("meth1", "Test.Sam1S")))
  val methSam1J = mkF("methSam1J", mkSam1(        q"val t5c: Sam1J     = meth1                   ", Sam1J, noMsgs))
  val prop1     = mkF("prop1",     mkTest(prop,   q"val t2a: () => Any = prop                    ", prop1Msgs))
  val prop2     = mkF("prop2",     mkTest(prop,   q"val t2b: Any       = { val t = prop   ; t }  ", noMsgs))
  val prop3     = mkF("prop3",     mkTest(prop,   q"val t2c: () => Any = prop()                  ", prop3Msgs))
  val prop4     = mkF("prop4",     mkTest(prop,   q"val t2d: () => Any = prop _                  ", prop4Msgs))
  val prop5     = mkF("prop5",     mkTest(prop,   q"val t2e: () => Any = { val t = prop _ ; t }  ", prop5Msgs))
  val prop6     = mkF("prop6",     mkTest(prop,   q"val t2f: Any       = prop _                  ", prop6Msgs))
  val prop7     = mkF("prop7",     mkTest(prop,   q"val t2g: Any       = prop() _                ", prop7Msgs))
  val methF0_1  = mkF("methF0_1",  mkTest(methF0, q"val t1a: () => Any = methF0                  ", noMsgs))
  val methF0_2  = mkF("methF0_2",  mkTest(methF0, q"val t1b: () => Any = { val t = methF0; t }   ", autoApp(q"methF0")))
  val methF0_3  = mkF("methF0_3",  mkTest(methF0, q"val t1c: () => Any = methF0 _                ", msgsForEtaX()))
  val methF0_4  = mkF("methF0_4",  mkTest(methF0, q"val t1d: Any       = methF0 _                ", msgsForEtaX()))
  val methF0_5  = mkF("methF0_5",  mkTest(methF0, q"val t1e: Any       = methF0() _              ", msgsFor2(_ => mustFollow("() => String")) ++ onlyFuncs("() => String")))
  val meth21    = mkF("meth21",    mkTest(meth2,  q"val t4a: () => Any = meth2                   ", msgsForEtaX()))
  val meth22    = mkF("meth22",    mkTest(meth2,  q"val t4b: () => Any = meth2()                 ", noMsgs))
  val meth23    = mkF("meth23",    mkTest(meth2,  q"val t4c: () => Any = meth2 _                 ", msgsForEtaX()))
  val meth24    = mkF("meth24",    mkTest(meth2,  q"val t4d: () => Any = meth2() _               ", msgsForEtaX()))

  val methT     = mkFile("EtaX.meth",  List(meth01, meth02, meth03, meth04, meth05, meth06))
  val meth1T    = mkFile("EtaX.meth1", List(meth11, meth12, meth13))
  val boom      = mkF("boom",  mkTest(q"class A { def boom(): Unit = () }", q"new A().boom", autoApp(q"boom")))
  val cloneEta  = mkF("clone", TestContents(Nil, List(q"val ys = { val t = scala.collection.mutable.Map(1 -> 'a'); t.clone }"), noMsgs))

  def mkF(name: String, t: Test)                          = TestFile(s"EtaX.$name", t)
  def mkSam0(stat: Stat, samDefn: Defn.Trait, msgs: Msgs) = TestContents(List(samDefn, meth ), List(stat), msgs)
  def mkSam1(stat: Stat, samDefn: Defn.Trait, msgs: Msgs) = TestContents(List(samDefn, meth1), List(stat), msgs)

  def msgsForEtaX(simple: Boolean = false) = {
    val solution = if (simple) "you can simply leave out the trailing ` _`" else "you can use `(() => <function>())` instead"
    val text     = s"The syntax `<function> _` is no longer supported;\n$solution"
    Msgs(Nil, Nil, Nil, Nil, List(Msg(W, text)), List(Msg(E, text)))
  }

  def noArgsList(meth: String, encl: String) = {
    val msg = err(s"""missing argument list for method $meth in $encl
                     |Unapplied methods are only converted to functions when a function type is expected.
                     |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin)
    Msgs(List(msg), Nil, Nil, Nil, Nil, Nil)
  }

  def sam0Msgs(encl: String) = {
    typeMismatches("String", encl).for2 ++ autoApp(q"meth").for2 ++ // in S2, type mismatch errors trump auto-app messages
    autoApp(q"meth").for3 ++ typeMismatches("String", encl).for3    // in S3, it's the reverse
  }

  def stillEta(meth: String, encl: String) = {
    msgsFor3(_ => Msg(W, s"method $meth is eta-expanded even though $encl does not have the @FunctionalInterface annotation."))
  }

  def noEtaNullary = msgsFor2 {
    case W => Msg(W, "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, write a function literal `() => m` instead")
    case E => Msg(E, "Methods without a parameter list and by-name params can not be converted to functions as `m _`, write a function literal `() => m` instead")
  }

  def missingArgs(meth: String, cls: String, param: String) =
    msgsFor2(_ => MissingArgMsg(E, s"not enough arguments for method $meth in class $cls.\nUnspecified value parameter $param.")) ++
    msgsFor3(_ => MissingArgMsg(E, s"missing argument for parameter $param of method $meth"))
}
