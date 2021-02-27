package shisa
package tests

import scala.meta._

import hedgehog._, core.Info, runner.{ Prop => _, _ }

//https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html
//https://www.scala-lang.org/files/archive/spec/2.13/02-identifiers-names-and-scopes.html
//https://jacobstanley.io/how-to-use-hedgehog-to-test-a-real-world-large-scale-stateful-app/
//https://github.com/hedgehogqa/scala-hedgehog/blob/master/doc/tutorial.md
//https://scalameta.org/docs/assets/tree.svg
//https://scalameta.org/docs/trees/quasiquotes.html
//https://scalameta.org/docs/trees/examples.html
//http://plastic-idolatry.com/erik/oslo2019.pdf
//  https://www.youtube.com/watch?v=O78hnJuzQwA
//https://slides.yowconference.com/yowlambdajam2017/Stanley-GensNRoses.pdf
//  https://www.youtube.com/watch?v=AIv_9T0xKEo
object ScalaMetaGens {
  //Letters, which include lower case letters (Ll), upper case letters (Lu), title case letters (Lt), other letters (Lo), modifier letters (Ml), letter numerals (Nl) and the two characters \u0024 ‘$’ and \u005F ‘_’.
  //val lowerAll = Gen.w
  //Gen.digit

  def genTermName: Gen[Term.Name] = {
    val charFreq = List(
      9 -> Gen.alpha,
      //7 -> Gen.alphaNum,
      //5 -> Gen.ascii,
      //3 -> Gen.latin1,
      //1 -> Gen.unicode,
    )
    val range = Range.linear(1, 99)
    Gen.choice1(
      Gen.string(Gen.frequencyUnsafe(charFreq), range),
      Gen.frequencyUnsafe(charFreq.map(_.map(Gen.string(_, range)))),
    ).map(Term.Name(_))
  }

  def genTypeName: Gen[Type.Name] = {
    val charFreq = List(
      9 -> Gen.alpha,
      //7 -> Gen.alphaNum,
      //5 -> Gen.ascii,
      //3 -> Gen.latin1,
      //1 -> Gen.unicode,
    )
    val range = Range.linear(1, 99)
    Gen.choice1(
      Gen.string(Gen.frequencyUnsafe(charFreq), range),
      Gen.frequencyUnsafe(charFreq.map(_.map(Gen.string(_, range)))),
    ).map(Type.Name(_))
  }

  def genTypeParam : Gen[Type.Param] = ???
  def genParam     : Gen[Term.Param] = ???
  def genType      : Gen[Type]       = ???
  def genBody      : Gen[Term]       = genTerm
  def genTerm      : Gen[Term]       = Gen.constant(Lit.String("a"))
  def genMod       : Gen[Mod]        = ???
  def genDefn      : Gen[Defn]       = ???

  def genDefnDef: Gen[Defn.Def] = for {
    mods    <- genNil[Mod]              // genMod.list(Range.linear(0, 9))
    name    <- genTermName
    tparams <- genNil[Type.Param]       // genTypeParam.list(Range.linear(0, 9))
    paramss <- genNil[List[Term.Param]] // genParam.list(Range.linear(0, 9)).list(Range.linear(0, 9))
    decltpe <- genNone[Type]            // genType.option
    body    <- genBody
  } yield Defn.Def(mods, name, tparams, paramss, decltpe, body)

  def genTopLevelStats: Gen[List[Stat]] = {
    Gen.frequency1(
      9 -> genNil,
      1 -> genPkg.list(Range.linear(0, 9)),
    )
  }

  def genPkg: Gen[Pkg] = {
    def genPkgName: Gen[Term.Ref] = {
      // TODO: package names can have numbers, underscore, upper-case, etc...
      def genName = for (s <- Gen.string(Gen.lower, Range.linear(1, 32))) yield Term.Name(s)
      Gen.choice1(
        genName,
        for (name <- genName; qual <- genPkgName) yield Term.Select(qual, name),
      )
    }

    val genPkgNames = Gen.frequency1(
      3 -> genPkgName.list(Range.singleton(1)),
      1 -> genPkgName.list(Range.linear(1, 9)),
    )

    for {
      pkgName :: pkgNames <- genPkgNames
      stats               <- genTopLevelStats
    } yield pkgNames.foldLeft(Pkg(pkgName, stats))((pkg, name) => Pkg(name, List(pkg)))
  }

  def genSource: Gen[Source] = for {
    stats <- Gen.frequency1[List[Stat]](
      1 -> genTopLevelStats, // in the empty package
      9 -> genPkg.upcast.list(Range.singleton(1)), // single
    )
  } yield Source(stats)

  def genNil[A]: Gen[List[A]]    = Gen.constant(Nil)
  def genNone[A]: Gen[Option[A]] = Gen.constant(None)

  def compiles2(testFile: TestFile): Result = compilesInternal(Main.SC2, testFile)
  def compiles3(testFile: TestFile): Result = compilesInternal(Main.SC3, testFile)

  def compilesInternal(mkCompiler: MkCompiler, testFile: TestFile): Result = {
    Main.runTest(List(mkCompiler.mkCompiler), testFile.name, testFile.test) match {
      case Nil      => Result.success
      case failures => Result.Failure(failures.map(tf => Info(tf.msg)))
    }
  }

  implicit def upcastGen[T, U >: T](gen: Gen[T]): Gen[U] = gen.upcast[U]

  implicit class GenOps[T](private val x: Gen[T]) extends AnyVal {
    def upcast[U >: T]: Gen[U] = x.map(x => x)
  }

  implicit class Tuple2Ops[A, B](private val t: (A, B)) extends AnyVal {
    def map[C](f: B => C): (A, C) = (t._1, f(t._2))
  }
}

object chars {
  val Space    = ' '      // aka \u0020
  val Tab      = '\u0009'
  val CR       = '\r'     // aka \u000D
  val NL       = '\n'     // aka \u000A
  val Dollar   = '$'      // aka \u0024
  val UScore   = '_'      // aka \u005F
  val LBrace   = '{'
  val LBracket = '['
  val LParen   = '('
  val RBrace   = '}'
  val RBracket = ']'
  val RParen   = ')'
  val BTick    = '`'
  val SQuote   = '\''
  val DQuote   = '"'
  val Dot      = '.'
  val Semi     = ';'
  val Comma    = ','
  val Bang     = '!'
  val Hash     = '#'
  val Percent  = '%'
  val Amper    = '&'
  val Asterisk = '*'
  val Plus     = '+'
  val Minus    = '-'
  val Divide   = '/'
  val Colon    = ':'
  val LT       = '<'
  val EQ       = '='
  val GT       = '>'
  val Qmark    = '?'
  val At       = '@'
  val BSlash   = '\\'
  val Caret    = '^'
  val Pipe     = '|'
  val Tilde    = '~'

  val ws = List(Space, Tab, CR, NL)

  val lower    = ('a' to 'z').toList
  val upper    = ('A' to 'Z').toList
  val title    = Nil // title case letters (Lt)
  val other    = Nil // other letters (Lo)
  val modifier = Nil // modifier letters (Ml)
  val numbers  = Nil // letter numerals (Nl)
  val letters  = lower ++ upper ++ title ++ other ++ modifier ++ numbers ++ List(Dollar, UScore)

  val parentheses = List(LParen, RParen, LBracket, RBracket, LBrace, RBrace)
  val delim       = List(BTick, SQuote, DQuote, Dot, Semi, Comma)
  val operator    = List(
    Bang, Hash, Percent, Amper, Asterisk, Plus, Minus, Divide,
    Colon, LT, EQ, GT, Qmark, At, BSlash, Caret, Pipe, Tilde,
  )
}
