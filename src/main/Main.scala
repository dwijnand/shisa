package shisa

import scala.language.implicitConversions

import java.io.{ File, PrintWriter }
import java.net.URLClassLoader
import java.nio.file._
import java.util.concurrent.Executors

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.chaining._

import scala.meta._

object Main {
  val cwdAbs    = Paths.get("").toAbsolutePath
  val targetDir = Paths.get("target")

  val dotcCp = BuildInfo.scalac3Dir +: Deps.scalac_3_00_base
  val dotcCl = new URLClassLoader(dotcCp.map(_.toURI.toURL).toArray, getClass.getClassLoader)
  val freshCompiler3Cls  = dotcCl.loadClass("shisa.FreshCompiler3")
  val freshCompiler3Ctor = freshCompiler3Cls.getConstructor(classOf[String], classOf[Array[File]], classOf[String])

  def FreshCompiler3(id: String, cmd: String): MkCompiler =
    freshCompiler3Ctor.newInstance(id, Deps.scalac_3_00_base.toArray, cmd).asInstanceOf[MkCompiler]

  val mkCompilers = Seq[MkCompiler](
    FreshCompiler2("2.13-base", Deps.scalac_2_13_base, ""),
    FreshCompiler2("2.13-head", Deps.scalac_2_13_head, ""),
    FreshCompiler2("2.13-new",  Deps.scalac_2_13_head, "-Xsource:3"),
    FreshCompiler3("3.0-old",                          "-source 3.0-migration"),
    FreshCompiler3("3.0",                              ""), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr",                         "-source 3.1-migration"),
    FreshCompiler3("3.1",                              "-source 3.1"),
  )

  val callHashHashTestFile = InMemoryTestFile(
    Paths.get("tests/Call.##.scala"),
    outerPrelude = Nil,
    innerPrelude = List[Defn](
      Defn.Val(Nil, List(Pat.Var(Term.Name("any"))), Some(Type.Name("Any")),    Lit.String("")),
      Defn.Val(Nil, List(Pat.Var(Term.Name("ref"))), Some(Type.Name("AnyRef")), Lit.String("")),
    ),
    testStats = List[List[Stat]](
      List[Stat](
        Term.Select(Term.Name("any"), Term.Name("##")),
        Term.Apply(Term.Select(Term.Name("any"), Term.Name("##")), Nil),
      ),
      List[Stat](
        Term.Select(Term.Name("ref"), Term.Name("##")),
        Term.Apply(Term.Select(Term.Name("ref"), Term.Name("##")), Nil),
      ),
    )
  )

  def main(args: Array[String]): Unit = {
    val sourceFiles = args.toList match {
      case Nil => Nil // Files.find(Paths.get("tests"), /* maxDepth = */ 10, (p, _) => s"$p".endsWith(".scala")).toScala(List).sorted
      case xs  => xs.map(Paths.get(_)).map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
    }

    if (Files.exists(Paths.get("target/tests")))
      IOUtil.deleteRecursive(Paths.get("target/tests"))

    val testFiles = sourceFiles.map(RealTestFile(_)) :+ callHashHashTestFile

    val pool    = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val futures = testFiles.map(f => pool.submit[Unit](() => compile1(f, mkCompilers)))

    pool.shutdown()

    if (!pool.awaitTermination(10, MINUTES))
      throw new Exception("Thread pool timeout elapsed before all tests were complete!")

    futures.foreach(_.get(0, NANOSECONDS))
  }

  def compile1(testFile: TestFile, mkCompilers: Seq[MkCompiler]) = {
    val sourceFile = testFile.src
    val compilers = mkCompilers.map(_.mkCompiler())

    if (sourceFile.toString.endsWith(".lines.scala")) {
      doCompileLines(sourceFile, compilers)
    } else {
      val src2 = Main.targetDir.resolve(sourceFile)
      Files.createDirectories(src2.getParent)
      testFile match {
        case RealTestFile(_)                   => Files.copy(sourceFile, src2)
        case tf @ InMemoryTestFile(_, _, _, _) => Files.writeString(src2, ShisaMeta.testFileSource(tf))
      }
      val results = compilers.map(doCompile(sourceFile, _))
      val lines   = Files.readString(src2).linesIterator.size
      println(f"> ${s"$sourceFile ($lines lines)"}%-45s ${statusLine(results)}")
    }
  }

  def doCompile(sourceFile: Path, compiler: Compiler) = {
    val file = CompileFile1(sourceFile, compiler.id)
    val res = compiler.compile1(file.src2)
    val writeBody = s"// exitCode: ${res.exitCode}" +: res.lines.asScala
    (writeBody.init :+ writeBody.last.stripLineEnd).foreach(file.chk.println)
    file.chk.close()
    res
  }

  val TestRegex = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r

  def doCompileLines(sourceFile: Path, compilers: Seq[Compiler]) = {
    val (setup, base, input) = Files.readString(sourceFile) match {
      case TestRegex(setup, base, cases) => (setup, base, cases.linesIterator.toList)
    }

    input.iterator.filter(!_.trim.pipe(isEmptyOrComment)).zipWithIndex.foreach { case (line, _idx) =>
      val file = CompileFileLine(sourceFile, _idx)

      val body = Array.fill(input.size)("")
      body(_idx) = line
      Files.writeString(file.src2, s"package p${file.idx}\n\n$setup\nclass Test $base{\n${body.mkString("\n")}\n}\n")

      val results = compilers.map(compiler => (compiler.id, compiler.compile1(file.src2)))

      file.chk.println(s"// src: $line")

      results.foldLeft(new CompileResult(-127, Nil.asJava)) { case (prevRes, (id, res)) =>
        val result = statusPadded(res)
        val writeBody = if (res == prevRes)
          Seq(f"// $id%-9s $result <no change>")
        else
          Seq(f"// $id%-9s $result".trim) ++ (if (res.lines.isEmpty) Nil else res.lines.asScala :+ "")
        writeBody.foreach(file.chk.println)
        res
      }

      file.chk.println()
      file.chk.println(results.map(x => statusPadded(x._2)).mkString(" ").trim)
      file.chk.close()

      val lineNo = setup.linesIterator.size + 2 + _idx
      println(f"> ${s"$sourceFile:$lineNo"}%-45s ${statusLine(results.map(_._2))}$line%-100s")
    }
  }

  def statusLine(xs: Seq[CompileResult]) = xs.iterator.map(statusIcon).mkString

  def statusIcon(res: CompileResult) = (res.exitCode, res.lines.asScala.toList) match {
    case (0, Nil) => s"${Console.GREEN}\u2713${Console.RESET}"  // check mark     (green)
    case (0, _)   => s"${Console.YELLOW}\u2623${Console.RESET}" // biohazard sign (yellow)
    case (_, _)   => s"${Console.RED}\u2717${Console.RESET}"    // cross mark     (red)
  }

  def statusPadded(res: CompileResult) = (res.exitCode, res.lines.asScala.toList) match {
    case (0, Nil) => "ok   "
    case (0, _)   => "warn "
    case (_, _)   => "error"
  }

  def isEmptyOrComment(s: String) = s.isEmpty || s.startsWith("//")
}

sealed trait TestFile { def src: Path }
final case class RealTestFile(src: Path) extends TestFile
final case class InMemoryTestFile(
    src: Path,
    outerPrelude: List[Defn],
    innerPrelude: List[Defn],
    testStats: List[List[Stat]],
) extends TestFile

sealed abstract class CompileFile(src: Path) {
  val name          = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
  val dir           = src.resolveSibling(name)
  def chkPath: Path
  lazy val chk      = new PrintWriter(Files.newBufferedWriter(chkPath), true)

  Files.createDirectories(dir)
}

final case class CompileFile1(src: Path, id: String) extends CompileFile(src) {
  val src2    = Main.targetDir.resolve(src)
  val chkPath = dir.resolve(s"$name.$id.check")
}

final case class CompileFileLine(src: Path, idxInt: Int) extends CompileFile(src) {
  val idx     = if (idxInt < 10) s"0$idxInt" else s"$idxInt"
  val src2    = Main.targetDir.resolve(src).resolveSibling(s"$name.$idx.scala")
  val chkPath = dir.resolve(s"$name.$idx.check")

  Files.createDirectories(src2.getParent)
}
