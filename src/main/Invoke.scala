package shisa

import java.nio.file.Path

trait Invoke {
  def id: String
  def cmd: String
  def mkRunner(): Runner

  def compile1(src: Path): CompileResult = mkRunner().compile1(src)
}
