package shisa

import scala.language.implicitConversions

import java.io.{ IOException, PrintWriter }
import java.nio.file._, attribute.BasicFileAttributes

import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters._
import scala.util.chaining._

object IOUtil {
  def createDirs(dir: Path) = {
    // Files.createDirectories returns the created directory...
    // but (sometimes? on Travis CI at least, compared to locally) as an absolute Path
    // so do this instead
    Files.createDirectories(dir)
    dir
  }

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
