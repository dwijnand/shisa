package shisa

import java.nio.file.Path

trait Runner {
  def compile1(src: Path): CompileResult
}
