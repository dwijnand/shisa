package shisa
package testdata

import java.nio.file._

import scala.meta._

trait MkInMemoryTestFile {
  def path: Path
  def outerPrelude: List[List[Defn]]
  def innerPrelude: List[Defn]
  def testStats: List[List[Stat]]

  def testFile = InMemoryTestFile(path, outerPrelude, innerPrelude, testStats)
}
