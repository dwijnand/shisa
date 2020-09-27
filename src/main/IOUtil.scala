package shisa

import scala.language.implicitConversions

import java.io.IOException
import java.nio.file._, attribute.BasicFileAttributes

object IOUtil {
  def deleteRecursive(p: Path): Unit = Files.walkFileTree(p, new DeleteVisitor)

  private final class DeleteVisitor extends SimpleFileVisitor[Path]() {
    override def visitFile(file: Path, attrs: BasicFileAttributes) = {
      Files.delete(file)
      FileVisitResult.CONTINUE
    }

    override def postVisitDirectory(dir: Path, exc: IOException) = {
      val listing = Files.list(dir)
      try if (!listing.iterator().hasNext())
        Files.delete(dir)
      finally listing.close()
      FileVisitResult.CONTINUE
    }
  }
}
