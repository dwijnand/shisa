package shisa

import scala.language.implicitConversions

import java.io.{ IOException, PrintWriter }
import java.nio.file._, attribute.BasicFileAttributes

import scala.collection.mutable.ListBuffer
import scala.jdk.StreamConverters._
import scala.util.chaining._

import ShisaIo._

object Main {
  def main(args: Array[String]): Unit = {
    val sourceFiles = args.toList match {
      case Nil  => Files.find(Paths.get("tests"), 10, (p, _) => p.toString.endsWith(".scala"))
        .toScala(List)
        .sorted
        .tap(xs => println(s"Files: ${xs.mkString("[", ", ", "]")}"))
      case argv  => argv.map(Paths.get(_)).map { p =>
        if (p.isAbsolute) Paths.get("").toAbsolutePath.relativize(p)
        else p
      }
    }

    val missing = sourceFiles.filter(!Files.exists(_))
    if (missing.nonEmpty)
      sys.error(s"Missing source files: ${missing.mkString("[", ", ", "]")}")

    val targetTestsDir = Paths.get("target/tests")
    if (Files.exists(targetTestsDir)) deleteRecursive(targetTestsDir)

    println(s"Combinations:${combinations.map(i => f"\n  ${i.id}%-9s : ${i.cmd}").mkString}")

    sourceFiles.foreach(InvokeCompiler.compileSwitch(_, combinations))
  }

  val combinations = Seq[Invoke](
    FreshCompiler2("2.13-base", Deps.lib_2_13_base, ""),
    FreshCompiler2("2.13-head", Deps.lib_2_13_head, ""),
    FreshCompiler2("2.13-new",  Deps.lib_2_13_head, "-Xsource:3"),
    FreshCompiler3("3.0-old",  "-source 3.0-migration"),
    FreshCompiler3("3.0",      ""), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr", "-source 3.1-migration"),
    FreshCompiler3("3.1",      "-source 3.1"),
  )
}

sealed abstract class CompileFile(val src: Path) {
  val name          = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
  val dir           = createDirs(src.resolveSibling(name))
  val targetDir     = Paths.get("target").resolve(dir)
  def chkPath: Path
  lazy val chk      = new PrintWriter(Files.newBufferedWriter(chkPath), true)
}

final case class CompileFile1(_src: Path, id: String) extends CompileFile(_src) {
  val out     = createDirs(targetDir.resolve(s"$name.$id"))
  val chkPath = dir.resolve(s"$name.$id.check")
}

final case class CompileFileLine(_src: Path, _idx: Int) extends CompileFile(_src) {
  val outD    = createDirs(targetDir.resolve(name))
  val idx     = if (_idx < 10) s"0${_idx}" else s"${_idx}"
  val chkPath = dir.resolve(s"$name.$idx.check")
  val src2    = outD.resolve(s"$name.$idx.scala")
  val out     = (id: String) => createDirs(outD.resolve(s"$name.$id.$idx"))
}

object InvokeCompiler {
  def compileSwitch(sourceFile: Path, combinations: Seq[Invoke]) = {
    print(s"> Testing $sourceFile")
    if (sourceFile.toString.endsWith(".lines.scala")) {
      println()
      doCompileLines(sourceFile, combinations)
    } else {
      print(" ")
      combinations.foreach(doCompile(sourceFile, _))
      println()
    }
  }

  def doCompile(sourceFile: Path, invoke: Invoke) = {
    val file = CompileFile1(sourceFile, invoke.id)
    val CompileResult(exitCode, lines) = invoke.compile1(file.src, file.out)
    val writeBody = s"// exitCode: $exitCode" +: lines
    (writeBody.init :+ writeBody.last.stripLineEnd).foreach(file.chk.println)
    file.chk.close()
    print('.')
  }

  def doCompileLines(sourceFile: Path, combinations: Seq[Invoke]) = {
    val re   = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r
    val (setup, base, input) = Files.readString(sourceFile) match {
      case re(setup0, base, cases) =>
        (setup0.linesIterator.map(_.trim).mkString("\n"), base, cases.linesIterator.toList)
    }
    def emptyOrCommented(s: String) = s.isEmpty || s.startsWith("//")
    input.iterator.zipWithIndex.filter(!_._1.trim.pipe(emptyOrCommented)).foreach { case (line, _idx) =>
      val file = CompileFileLine(sourceFile, _idx)
      print(s"    Testing ${file.src2} ")

      val body = Array.fill(input.size)("")
      body(_idx) = line
      Files.writeString(file.src2, s"$setup\nclass Test $base{\n${body.mkString("\n")}\n}\n")

      file.chk.println(s"// src: $line")

      val summaries = ListBuffer.empty[String]
      var prevRes = CompileResult(-127, Nil)
      combinations.foreach { invoke =>
        val id = invoke.id
        val res = invoke.compile1(file.src2, file.out(id))
        val result = res.statusPadded
        val writeBody = if (res == prevRes)
          Seq(f"// $id%-9s $result <no change>")
        else
          Seq(f"// $id%-9s $result") ++ (if (res.lines.isEmpty) Nil else res.lines :+ "")
        writeBody.foreach(file.chk.println)
        prevRes = res
        summaries += result
        print('.')
      }

      file.chk.println()
      file.chk.println(summaries.mkString(" "))
      file.chk.close()
      println()
    }
  }
}

object ShisaIo {
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
