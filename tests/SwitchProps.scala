package shisa
package tests

import hedgehog._
import hedgehog.core.Info
import hedgehog.runner.{ Prop => _, _ }

import shisa.Main._
import shisa.Switch._
import shisa.tests.ScalaMetaGens._

object SwitchProps extends Properties {
  def tests = List(
    //property("switch", switchProp).withTests(8),
  )

  def switchProp: Property = for {
       switch <- Gen.element1(Meth2Prop, Prop2Meth).forAll
                    //.cover(1, "Meth2Prop", { case Meth2Prop => true case Prop2Meth => false })
                    //.cover(1, "Prop2Meth", { case Prop2Meth => true case Meth2Prop => false })
         call <- Gen.element1(Meth, Prop).forAll
                    //.cover(1, "Meth", { case Meth => true case Prop => false })
                    //.cover(1, "Prop", { case Prop => true case Meth => false })
      clsName <- genTypeName.forAll
    traitName <- genTypeName.filter(x => x.value != clsName.value).forAll
         meth <- genTermName.forAll
      valName <- genTermName.forAll
         isVC <- Gen.element1(false, true).forAll
                    //.cover(1, "AnyRef class", _ == false)
                    //.cover(1, "AnyVal class", _ == true)
  } yield {
    val testFile = switchFile(switch, call, clsName, traitName, meth, valName, isVC)
    compile(testFile)
  }

  def compile(testFile: TestFile): Result = {
    val TestFile(name, test) = testFile
    Main.compile1(name, test.asInstanceOf[TestContents]) match {
      case TestSuccess(_)            => Result.success
      case TestFailure(_, msg)       => Result.Failure(List(Info(msg)))
      case TestFailures(_, failures) => Result.Failure(failures.map(tf => Info(tf.msg)))
    }
  }
}
