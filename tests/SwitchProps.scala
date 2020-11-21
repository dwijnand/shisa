package shisa
package tests

import hedgehog._
import hedgehog.core.{ Result => _, _ }
import hedgehog.runner.{ Prop => _, _ }

import shisa.testdata.Switch._
import shisa.tests.ScalaMetaGens._

object SwitchProps extends Properties {
  def tests = List(
    property("switch", switchProp).withTests(20),
  )

  def switchProp: Property = for {
    switchFile <- genSwitchFile.forAll
        .cover(1, "m2p_m",    asSf { case SwitchFile(Meth2Prop, Meth, _, _, _, _, false) => })
        .cover(1, "m2p_p",    asSf { case SwitchFile(Meth2Prop, Prop, _, _, _, _, false) => })
        .cover(1, "p2m_m",    asSf { case SwitchFile(Prop2Meth, Meth, _, _, _, _, false) => })
        .cover(1, "p2m_p",    asSf { case SwitchFile(Prop2Meth, Prop, _, _, _, _, false) => })
        .cover(1, "m2p_m_vc", asSf { case SwitchFile(Meth2Prop, Meth, _, _, _, _,  true) => })
        .cover(1, "m2p_p_vc", asSf { case SwitchFile(Meth2Prop, Prop, _, _, _, _,  true) => })
        .cover(1, "p2m_m_vc", asSf { case SwitchFile(Prop2Meth, Meth, _, _, _, _,  true) => })
        .cover(1, "p2m_p_vc", asSf { case SwitchFile(Prop2Meth, Prop, _, _, _, _,  true) => })
  } yield compile1(switchFile.toTestFile).toResult

  def genSwitchFile = for {
       switch <- Gen.element1(Meth2Prop, Prop2Meth)
         call <- Gen.element1(Meth, Prop)
         isVC <- Gen.element1(false, true)
      clsName <- genTypeName
    traitName <- genTypeName.filter(_.value != clsName.value)
         meth <- genTermName
      valName <- genTermName
  } yield SwitchFile(switch, call, clsName, traitName, meth, valName, isVC)

  def asSf(pf: PartialFunction[SwitchFile, Unit]): SwitchFile => Cover = pf.isDefinedAt(_)

  def compile1(testFile: TestFile) = Main.compile1(testFile)
  //def compile1(testFile: TestFile) = TestSuccess(testFile.name)

  implicit class TestResultOps(private val tr: TestResult) extends AnyVal {
    def toResult: Result = tr match {
      case TestSuccess(_)            => Result.success
      case TestFailures(_, failures) => Result.Failure(failures.map(tf => Info(tf.msg)))
      case tf @ TestFailure(name, _) => TestFailures(name, List(tf)).toResult
    }
  }

}
