package shisa
package testdata

import java.nio.file._

import scala.meta._, contrib._

object EtaX {
  object meth2 extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.meth2.lines.scala")
    val baseClass    = q"""class TestBase {
                             def meth2()() = ""
                           }"""
    val testStats    = List(
      q"val t4a: () => Any = meth2     // eta-expansion, but lint warning",
      q"val t4b: () => Any = meth2()   // ditto",
      q"val t4c: () => Any = meth2 _   // ok",
      q"val t4d: () => Any = meth2() _ // ok",
    )
    val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, Nil, Nil) // TODO
    val contents     = TestContents(Nil, Some(baseClass), Nil, List(testStats), expectedMsgs)
  }

  object boom extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.boom.lines.scala")
    val outerDefn    = q"class A { def boom(): Unit = () }"
    val testStat     = q"new A().boom" // ?/?/err: apply, ()-insertion
    val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, Nil, Nil) // TODO
    val contents     = TestContents(List(List(outerDefn)), None, Nil, List(List(testStat)), expectedMsgs)
  }
}
