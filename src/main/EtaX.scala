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

  val typeMismatch2_A = typeMismatch2("String", "() => Any")
  val typeMismatch3_A = typeMismatch3("String", "() => Any")
  val typeMismatch3_B = typeMismatch3("(t : String)", "() => Any")
  val missingArg2_A   = missingArg2("apply: (i: Int): Char", "StringOps", "i")
  val missingArg3_B   = missingArg3("apply: (i: Int): Char", "i")

  // TODO:
  // * Msgs
  // * build intelligent concat
  // * typeMismatch (err) trumps autoApp
  // * an autoApp err trumps override msg
  // * error adds context in 2?
  // * only funcs error trumps missing args, but not type mismatch

  def sam0Msgs(samDefn: Defn.Trait) = List(
    List(typeMismatch2("String", s"Test.${samDefn.name}")),
    List(typeMismatch2("String", s"Test.${samDefn.name}")),
    List(Msg(W, autoApp3("meth")), typeMismatch3("String", s"Test.${samDefn.name}")),
    List(Msg(E, autoApp3("meth"))),
    List(Msg(E, autoApp3("meth"))),
    List(Msg(E, autoApp3("meth"))),
  )

  val msgsProp1             = msgs2or3(_ => typeMismatch2_A, _ => typeMismatch3_A)
  val msgsProp3             = msgs2or3(_ =>   missingArg2_A, _ =>   missingArg3_B)
  val msgsProp6             = msgs2or3(methodsWithoutParams, onlyFuncs(_, "String"))
  def mustFuncs(tp: String) = msgs2or3(_ => mustFollow(tp),  onlyFuncs(_, tp))

  val msgsProp4 = List(
    List(methodsWithoutParams(W)),
    List(methodsWithoutParams(E)),
    List(onlyFuncs(W, "String")),
    List(onlyFuncs(E, "String")),
    List(onlyFuncs(W, "String"), typeMismatch3_A),
    List(onlyFuncs(E, "String")),
  )

  val msgsProp5 = List(
    List(methodsWithoutParams(W)),
    List(methodsWithoutParams(E)),
    List(onlyFuncs(W, "String")),
    List(onlyFuncs(E, "String"), typeMismatch3_B),
    List(onlyFuncs(W, "String"), typeMismatch3_B),
    List(onlyFuncs(E, "String"), typeMismatch3_B),
  )

  val msgsProp7 = List(
    List(missingArg2_A),
    List(missingArg2_A),
    List(onlyFuncs(W, "<error unspecified error>"), missingArg3_B),
    List(onlyFuncs(E, "<error unspecified error>")),
    List(onlyFuncs(W, "<error unspecified error>"), missingArg3_B),
    List(onlyFuncs(E, "<error unspecified error>")),
  )

  val msgsForEtaX1 = msgsFor31(Msg(_, "The syntax `<function> _` is no longer supported;\nyou can use `(() => <function>())` instead"))
  val msgsForEtaX2 = msgsFor31(Msg(_, "The syntax `<function> _` is no longer supported;\nyou can simply leave out the trailing ` _`"))

  def noArgsList(meth: String, encl: String) = {
    msgsFor2R(err(s"""missing argument list for method $meth in $encl
                     |Unapplied methods are only converted to functions when a function type is expected.
                     |You can make this conversion explicit by writing `$meth _` or `$meth(_)` instead of `$meth`.""".stripMargin))
  }

  def stillEta(meth: String, encl: String) =
    msgsFor3(Msg(W, s"method $meth is eta-expanded even though $encl does not have the @FunctionalInterface annotation."))

  val meth01    =                  mkTest(meth,   q"val t3a: () => Any  = meth                   ", noMsgs)
  val meth02    =                  mkTest(meth,   q"val t3b: Any        = { val t = meth; t }    ", autoApp(q"meth"))
  val meth03    =                  mkTest(meth,   q"val t3c: () => Any  = meth _                 ", msgsForEtaX1)
  val meth04    =                  mkTest(meth,   q"val t3d: () => Any  = { val t = meth _ ; t } ", msgsForEtaX1)
  val meth05    =                  mkTest(meth,   q"val t3e: Any        = meth _                 ", msgsForEtaX1)
  val meth06    =                  mkTest(meth,   q"val t3f: Any        = meth() _               ", mustFuncs("String"))
  val meth11    =                  mkTest(meth1,  q"val t5a: Any => Any = meth1                  ", noMsgs)
  val meth12    =                  mkTest(meth1,  q"val t5d: Any => Any = { val t = meth1   ; t }", noArgsList("meth1", "object Test"))
  val meth13    =                  mkTest(meth1,  q"val t5e: Any => Any = { val t = meth1 _ ; t }", msgsForEtaX2)
  val methSam0S = mkF("methSam0S", mkSam0(        q"val t3Sam0S: Sam0S = meth                    ", Sam0S))
  val methSam0J = mkF("methSam0J", mkSam0(        q"val t3Sam0J: Sam0J = meth                    ", Sam0J))
  val methSam1S = mkF("methSam1S", mkSam1(        q"val t5b: Sam1S     = meth1                   ", Sam1S, stillEta("meth1", "Test.Sam1S")))
  val methSam1J = mkF("methSam1J", mkSam1(        q"val t5c: Sam1J     = meth1                   ", Sam1J, noMsgs))
  val prop1     = mkF("prop1",     mkTest(prop,   q"val t2a: () => Any = prop                    ", msgsProp1))
  val prop2     = mkF("prop2",     mkTest(prop,   q"val t2b: Any       = { val t = prop   ; t }  ", noMsgs))
  val prop3     = mkF("prop3",     mkTest(prop,   q"val t2c: () => Any = prop()                  ", msgsProp3))
  val prop4     = mkF("prop4",     mkTest(prop,   q"val t2d: () => Any = prop _                  ", msgsProp4))
  val prop5     = mkF("prop5",     mkTest(prop,   q"val t2e: () => Any = { val t = prop _ ; t }  ", msgsProp5))
  val prop6     = mkF("prop6",     mkTest(prop,   q"val t2f: Any       = prop _                  ", msgsProp6))
  val prop7     = mkF("prop7",     mkTest(prop,   q"val t2g: Any       = prop() _                ", msgsProp7))
  val methF0_1  = mkF("methF0_1",  mkTest(methF0, q"val t1a: () => Any = methF0                  ", noMsgs))
  val methF0_2  = mkF("methF0_2",  mkTest(methF0, q"val t1b: () => Any = { val t = methF0; t }   ", autoApp(q"methF0")))
  val methF0_3  = mkF("methF0_3",  mkTest(methF0, q"val t1c: () => Any = methF0 _                ", msgsForEtaX1))
  val methF0_4  = mkF("methF0_4",  mkTest(methF0, q"val t1d: Any       = methF0 _                ", msgsForEtaX1))
  val methF0_5  = mkF("methF0_5",  mkTest(methF0, q"val t1e: Any       = methF0() _              ", mustFuncs("() => String")))
  val meth21    = mkF("meth21",    mkTest(meth2,  q"val t4a: () => Any = meth2                   ", msgsForEtaX1))
  val meth22    = mkF("meth22",    mkTest(meth2,  q"val t4b: () => Any = meth2()                 ", noMsgs))
  val meth23    = mkF("meth23",    mkTest(meth2,  q"val t4c: () => Any = meth2 _                 ", msgsForEtaX1))
  val meth24    = mkF("meth24",    mkTest(meth2,  q"val t4d: () => Any = meth2() _               ", msgsForEtaX1))

  val methT     = mkFile("EtaX.meth",  List(meth01, meth02, meth03, meth04, meth05, meth06))
  val meth1T    = mkFile("EtaX.meth1", List(meth11, meth12, meth13))
  val boom      = mkF("boom",  mkTest(q"class A { def boom(): Unit = () }", q"new A().boom", autoApp(q"boom")))
  val cloneEta  = mkF("clone", TestContents(Nil, List(q"val ys = { val t = scala.collection.mutable.Map(1 -> 'a'); t.clone }"), noMsgs))

  def mkF(name: String, t: Test)                                     = TestFile(s"EtaX.$name", t)
  def mkSam0(stat: Stat, samDefn: Defn.Trait)                        = TestContents(List(samDefn, meth ), List(stat), sam0Msgs(samDefn))
  def mkSam1(stat: Stat, samDefn: Defn.Trait, msgs: List[List[Msg]]) = TestContents(List(samDefn, meth1), List(stat), msgs)

  def typeMismatch2(tp: String, pt: String) = err(s"type mismatch;\n found   : $tp\n required: $pt")
  def typeMismatch3(tp: String, pt: String) = err(s"Found:    $tp\nRequired: $pt")

  def mustFollow(tp: String)          = err(s"_ must follow method; cannot follow $tp")
  def onlyFuncs(sev: Sev, tp: String) = Msg(sev, s"Only function types can be followed by _ but the current expression has type $tp")

  def methodsWithoutParams(sev: Sev) = sev match {
    case W => Msg(sev, "Methods without a parameter list and by-name params can no longer be converted to functions as `m _`, write a function literal `() => m` instead")
    case E => Msg(sev, "Methods without a parameter list and by-name params can not be converted to functions as `m _`, write a function literal `() => m` instead")
  }

  def missingArg2(meth: String, cls: String, param: String) = err(s"not enough arguments for method $meth in class $cls.\nUnspecified value parameter $param.")
  def missingArg3(meth: String, param: String)              = err(s"missing argument for parameter $param of method $meth")
}
