package shisa

import scala.meta._, contrib._

object EtaX {
  def tests: List[TestFile] = List(boom, cloneEta,
    meth21, meth22, meth23, meth24,
    methF0_1, methF0_2, methF0_3, methF0_4, methF0_5,
    prop1, prop2, prop3, prop4, prop5, prop6, prop7,
    meth1T, methSam1S, methSam1J,
    methT, methSam0S, methSam0J,
  )

  val meth   = q"def meth()        = ${Lit.String("")}"
  val meth1  = q"def meth1(x: Any) = ${Lit.String("")}"
  val methF0 = q"def methF0()      = () => ${Lit.String("")}"
  val meth2  = q"def meth2()()     = ${Lit.String("")}"
  val prop   = q"def prop = ${Lit.String("")}"
  val Sam0S  = q"                     trait Sam0S { def apply(): Any }"
  val Sam0J  = q"@FunctionalInterface trait Sam0J { def apply(): Any }"
  val Sam1S  = q"                     trait Sam1S { def apply(x: Any): Any }"
  val Sam1J  = q"@FunctionalInterface trait Sam1J { def apply(x: Any): Any }"

  val meth01    =                  mkTest(meth,   q"val t3a: () => Any  = meth                   ", noMsgs)
  val meth02    =                  mkTest(meth,   q"val t3b: Any        = { val t = meth; t }    ", autoApp(q"object Test", q"meth"))
  val meth03    =                  mkTest(meth,   q"val t3c: () => Any  = meth _                 ", msgsForEtaX())
  val meth04    =                  mkTest(meth,   q"val t3d: () => Any  = { val t = meth _ ; t } ", msgsForEtaX())
  val meth05    =                  mkTest(meth,   q"val t3e: Any        = meth _                 ", msgsForEtaX())
  val meth06    =                  mkTest(meth,   q"val t3f: Any        = meth() _               ", msgsFor2(_ => mustFollow("String")) ++ onlyFuncs("String"))
  val meth11    =                  mkTest(meth1,  q"val t5a: Any => Any = meth1                  ", noMsgs)
  val meth12    =                  mkTest(meth1,  q"val t5d: Any => Any = { val t = meth1   ; t }", noArgsList("meth1", "object Test"))
  val meth13    =                  mkTest(meth1,  q"val t5e: Any => Any = { val t = meth1 _ ; t }", msgsForEtaX(simple = true))
  val methSam0S = mkF("methSam0S", mkSam0(meth,   q"val t3Sam0S: Sam0S = meth             ", Sam0S, sam0Msgs("Test.Sam0S")))
  val methSam0J = mkF("methSam0J", mkSam0(meth,   q"val t3Sam0J: Sam0J = meth             ", Sam0J, sam0Msgs("Test.Sam0J")))
  val methSam1S = mkF("methSam1S", mkSam1(meth1,  q"val t5b: Sam1S     = meth1            ", Sam1S, stillEta("meth1", "Test.Sam1S")))
  val methSam1J = mkF("methSam1J", mkSam1(meth1,  q"val t5c: Sam1J     = meth1            ", Sam1J, noMsgs))
  val prop1     = mkF("prop1",     mkTest(prop,   q"val t2a: () => Any = prop                    ", typeMismatches("String", "() => Any")))
  val prop2     = mkF("prop2",     mkTest(prop,   q"val t2b: Any       = { val t = prop   ; t }  ", noMsgs))
  val prop3     = mkF("prop3",     mkTest(prop,   q"val t2c: () => Any = prop()                  ", missingArgs("StringOps", "apply", "i", "(i: Int): Char")))
  val prop4     = mkF("prop4",     mkTest(prop,   q"val t2d: () => Any = prop _                  ", noEtaNullary ++ onlyFuncs("String") ++ etaMismatch("String", "() => Any")))
  val prop5     = mkF("prop5",     mkTest(prop,   q"val t2e: () => Any = { val t = prop _ ; t }  ", noEtaNullary ++ onlyFuncs("String") ::: etaMismatch("(t : String)", "() => Any")))
  val prop6     = mkF("prop6",     mkTest(prop,   q"val t2f: Any       = prop _                  ", noEtaNullary ++ onlyFuncs("String")))
  val prop7     = mkF("prop7",     mkTest(prop,   q"val t2g: Any       = prop() _                ", onlyFuncs("<error unspecified error>") ++ missingArgs("StringOps", "apply", "i", "(i: Int): Char")))
  val methF0_1  = mkF("methF0_1",  mkTest(methF0, q"val t1a: () => Any = methF0                  ", noMsgs))
  val methF0_2  = mkF("methF0_2",  mkTest(methF0, q"val t1b: () => Any = { val t = methF0; t }   ", autoApp(q"object Test", q"methF0")))
  val methF0_3  = mkF("methF0_3",  mkTest(methF0, q"val t1c: () => Any = methF0 _                ", msgsForEtaX()))
  val methF0_4  = mkF("methF0_4",  mkTest(methF0, q"val t1d: Any       = methF0 _                ", msgsForEtaX()))
  val methF0_5  = mkF("methF0_5",  mkTest(methF0, q"val t1e: Any       = methF0() _              ", msgsFor2(_ => mustFollow("() => String")) ++ onlyFuncs("() => String")))
  val meth21    = mkF("meth21",    mkTest(meth2,  q"val t4a: () => Any = meth2                   ", autoApp(q"object Test", q"meth2").for3))
  val meth22    = mkF("meth22",    mkTest(meth2,  q"val t4b: () => Any = meth2()                 ", noMsgs))
  val meth23    = mkF("meth23",    mkTest(meth2,  q"val t4c: () => Any = meth2 _                 ", autoApp(q"object Test", q"meth2").for3))
  val meth24    = mkF("meth24",    mkTest(meth2,  q"val t4d: () => Any = meth2() _               ", msgsForEtaX()))

  val methT     = mkFile("EtaX.meth",  List(meth01, meth02, meth03, meth04, meth05, meth06))
  val meth1T    = mkFile("EtaX.meth1", List(meth11, meth12, meth13))
  val boom      = mkF("boom",  mkTest(q"class A { def boom(): Unit = () }", q"new A().boom", autoApp(q"class A", q"boom")))
  val cloneEta  = mkF("clone", TestContents(Nil, List(q"val ys = { val t = scala.collection.mutable.Map(1 -> 'a'); t.clone }"), noMsgs))

  def mkF(name: String, t: Test)                          = TestFile(s"EtaX.$name", t)
  def mkSam0(defn: Defn, stat: Stat, samDefn: Defn.Trait, msgs: Msgs) = TestContents(List(samDefn, defn), List(stat), msgs)
  def mkSam1(defn: Defn, stat: Stat, samDefn: Defn.Trait, msgs: Msgs) = TestContents(List(samDefn, defn), List(stat), msgs)

  def msgsForEtaX(simple: Boolean = false) = {
    val solution = if (simple) "you can simply leave out the trailing ` _`" else "you can use `(() => <function>())` instead"
    val text     = s"The syntax `<function> _` is no longer supported;\n$solution"
    Msgs(Nil, Nil, Nil, Nil, List(Msg(W, text)), List(Msg(E, text)))
  }

  def mustFollow(tp: String) = err(s"_ must follow method; cannot follow $tp")
  def onlyFuncs(tp: String)  = msgsFor3(Msg(_, s"Only function types can be followed by _ but the current expression has type $tp"))

  def noArgsList(meth: String, encl: String) = {
    val msg = err(s"""missing argument list for method $meth in $encl
                     |Unapplied methods are only converted to functions when a function type is expected.
                     |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin)
    Msgs(List(msg), Nil, Nil, Nil, Nil, Nil)
  }

  def sam0Msgs(encl: String) = {
    (typeMismatches("String", encl) ++ autoApp(q"object Test", q"meth")).for2 ++ // in S2, type mismatch errors trump auto-app messages
    (autoApp(q"object Test", q"meth") ++ typeMismatches("String", encl)).for3    // in S3, it's the reverse
  }

  def stillEta(meth: String, encl: String) = {
    msgsFor3(_ => Msg(W, s"method $meth is eta-expanded even though $encl does not have the @FunctionalInterface annotation."))
  }

  def noEtaNullary = msgsFor2 {
    case W => Msg(W, "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, write a function literal `() => m` instead")
    case E => Msg(E, "Methods without a parameter list and by-name params can not be converted to functions as `m _`, write a function literal `() => m` instead")
  }

  def missingArgs(cls: String, meth: String, param: String, sig: String) =
    msgsFor2(_ => Msg(E, s"not enough arguments for method $meth: $sig in class $cls.\nUnspecified value parameter $param.")) ++
    msgsFor3(_ => Msg(E, s"missing argument for parameter $param of method $meth in class $cls: $sig"))

  def typeMismatch2(tp: String, pt: String)  = Msg(E, s"type mismatch;\n found   : $tp\n required: $pt")
  def typeMismatch3(tp: String, pt: String)  = Msg(E, s"Found:    $tp\nRequired: $pt")
  def typeMismatches(tp: String, pt: String) = msgsFor2(_ => typeMismatch2(tp, pt)) ++ msgsFor3(_ => typeMismatch3(tp, pt))
  def etaMismatch(tp: String, pt: String)    = Msgs(Nil, Nil, Nil, List(typeMismatch3(tp, pt)), List(typeMismatch3(tp, pt)), List(typeMismatch3(tp, pt)))
}
