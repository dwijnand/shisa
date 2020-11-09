package shisa
package tests

import hedgehog.{ Syntax => _, _ }
import hedgehog.core.{ Info, PropertyT }
import hedgehog.runner._
import shisa.testdata.Switch.{ Meth2Prop, Prop2Meth }

import scala.meta._
import scala.tools.nsc.{ Global, Settings }
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.StoreReporter

object SwitchProps extends Properties {
  lazy val settings = {
    val s = new Settings()
    s.embeddedDefaults[SwitchProps.type]
    s.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
    s
  }

  lazy val reporter = new StoreReporter
  lazy val g        = new Global(settings, reporter)

  import shisa.testdata.Switch._

  def tests = List(
    property("switch", switchProp).withTests(3),
  )

  def switchProp: Property = {
    val genSwitchFile = for {
         switch <- Gen.element1(Meth2Prop, Prop2Meth)
           call <- Gen.element1(Meth, Prop)
        clsName <- genTypeName
      traitName <- genTypeName.filter(_.value != clsName.value)
           meth <- genTermName
        valName <- genTermName
           isVC <- Gen.element1(false, true)
    } yield switchFile(switch, call, clsName, traitName, meth, valName, isVC)
    compiles(genSwitchFile)
  }

  def compiles(genTestFile: Gen[TestFile]): Property = {
    for (testFile <- genTestFile.forAll) yield {
      Main.compile1(testFile) match {
        case TestSuccess(_)            => Result.success
        case TestFailure(_, msg)       => Result.Failure(List(Info(msg)))
        case TestFailures(_, failures) => Result.Failure(failures.map(tf => Info(tf.msg)))
      }
    }
  }

  def genSource: Gen[Source] = {
    for {
      stats <- Gen.frequency1[List[Stat]](
        1 -> genTopLevelStats, // in the empty package
        9 -> genPkg.upcast.list(Range.singleton(1)), // single
      )
    } yield Source(stats)
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

  def genTopLevelStats: Gen[List[Stat]] = {
    Gen.frequency1(
      9 -> genNil,
      1 -> genPkg.list(Range.linear(0, 9)),
    )
  }

  def genDefn: Gen[Defn] = ???

  def genDefnDef: Gen[Defn.Def] = {
    for {
      mods    <- genNil[Mod]              // genMod.list(Range.linear(0, 9))
      name    <- genTermName
      tparams <- genNil[Type.Param]       // genTypeParam.list(Range.linear(0, 9))
      paramss <- genNil[List[Term.Param]] // genParam.list(Range.linear(0, 9)).list(Range.linear(0, 9))
      decltpe <- genNone[Type]            // genType.option
      body    <- genBody
    } yield Defn.Def(mods, name, tparams, paramss, decltpe, body)
  }

  def genMod: Gen[Mod] = ???

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

  def genBody: Gen[Term] = genTerm

  def genTerm: Gen[Term] = Gen.constant(Lit.String("a"))

  def genNil[A]: Gen[List[A]]    = Gen.constant(Nil)
  def genNone[A]: Gen[Option[A]] = Gen.constant(None)

  implicit def upcastGen[T, U >: T](gen: Gen[T]): Gen[U] = gen.upcast[U]

  implicit class GenOps[T](private val x: Gen[T]) extends AnyVal {
    def upcast[U >: T]: Gen[U] = x.map(x => x)
    //def upcast[U >: T]: Gen[U] = x.asInstanceOf[Gen[U]]
  }

  implicit class Tuple2Ops[A, B](private val t: (A, B)) extends AnyVal {
    def map[C](f: B => C): (A, C) = (t._1, f(t._2))
  }
}
