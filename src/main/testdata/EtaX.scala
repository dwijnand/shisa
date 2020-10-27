package shisa
package testdata

import java.nio.file._

import scala.meta._, contrib._

object EtaX {
  object boom extends MkInMemoryTestFile {
    val path         = Paths.get("testdata/EtaX/EtaX.boom.lines.scala")
    val outerDefn    = q"class A { def boom(): Unit = () }"
    val testStat     = q"new A().boom" // ?/?/err: apply, ()-insertion
    val expectedMsgs = List(Nil, Nil, Nil, Nil, Nil, Nil, Nil) // TODO
    val contents     = TestContents(List(List(outerDefn)), Nil, List(List(testStat)), expectedMsgs)
  }
}
