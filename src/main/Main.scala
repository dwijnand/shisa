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

import cats.data.Chain

import scala.meta._

import shisa.testdata._

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

  val inMemoryTestFiles = List(Call.hashHash, Call.pos).map(_.testFile)
  val inMemoryTestFilesMap = inMemoryTestFiles.map(tf => tf.src -> tf).toMap

  def main(args: Array[String]): Unit = {
    val testFiles = args.toList match {
      case Nil =>
        val realTestFiles = Files.find(Paths.get("testdata"), 10, (p, _) => s"$p".endsWith(".scala")).toScala(List)
          .map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
          .map(RealTestFile(_))
        (realTestFiles ::: inMemoryTestFiles).sortBy(_.src)
          .tap(fs => println(s"Files: ${fs.map(_.src).mkString("[", ", ", "]")}"))
      case xs  => xs
        .map(Paths.get(_))
        .map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
        .map(p => inMemoryTestFilesMap.getOrElse(p, RealTestFile(p)))
    }

    val missing = testFiles.collect { case RealTestFile(p) if !Files.exists(p) => p }
    if (missing.nonEmpty)
      sys.error(s"Missing test files: ${missing.mkString("[", ", ", "]")}")

    if (Files.exists(Paths.get("target/testdata")))
      IOUtil.deleteRecursive(Paths.get("target/testdata"))

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
      val results = compilers.map(doCompile(sourceFile, _)).map(resToStatus)
      val lines   = Files.readString(src2).linesIterator.size
      println(f"> ${s"$sourceFile ($lines lines)"}%-45s ${statusLine(results)}")
    }
  }

  def doCompile(sourceFile: Path, compiler: Compiler) = {
    val file = CompileFile1(sourceFile, compiler.id)
    val res = compiler.compile1(file.src2)
    file.writeLines((s"// exitCode: ${res.exitCode}" +: res.lines.asScala).toList)
    res
  }

  val TestRegex = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r

  def doCompileLines(sourceFile: Path, compilers: Seq[Compiler]) = {
    val (setup, base, input) = Files.readString(sourceFile) match {
      case TestRegex(setup, base, cases) => (setup, base, cases.linesIterator.toList)
    }

    input.iterator.filter(!_.trim.pipe(isEmptyOrComment)).zipWithIndex.foreach { case (line, _idx) =>
      val file = CompileFileLine(sourceFile, _idx)
      val body = List.fill(_idx)("") ::: line :: List.fill(input.size - _idx - 1)("")
      val code = List(s"package p${file.idx}", "", setup, s"class Test $base{") ::: body ::: List("}", "")

      Files.createDirectories(file.src2.getParent)
      Files.writeString(file.src2, code.mkString("\n"))

      val resultsWithId = compilers.map(compiler => (compiler.id, resToStatus(compiler.compile1(file.src2))))
      val results       = resultsWithId.map(_._2)

      val initialRes = (CompileWarn(Nil): CompileStatus, Chain.empty[String])
      val (_, lines) = resultsWithId.foldLeft(initialRes) { case ((prevRes, lines), (id, res)) =>
        val resStart = s"// ${id.padTo(9, ' ')} ${res.toStatusPadded}"
        val resLines = if (res.lines.isEmpty) Chain.empty else Chain.fromSeq(res.lines) :+ ""
        val newLines = if (res == prevRes) s"$resStart <no change>" +: Chain.empty else s"$resStart".trim +: resLines
        (res, lines ++ newLines)
      }

      val statusSummary = results.map(_.toStatusPadded).mkString(" ").trim

      file.writeLines((s"// src: $line" +: lines :+ "" :+ statusSummary).toList)

      val lineNo = setup.linesIterator.size + 2 + _idx
      println(f"> ${s"$sourceFile:$lineNo"}%-45s ${statusLine(results)}$line%-100s")
    }
  }

  def resToStatus(res: CompileResult) = (res.exitCode, res.lines.asScala.toList) match {
    case (0, Nil)   => CompileOk
    case (0, lines) => CompileWarn(lines)
    case (_, lines) => CompileErr(lines)
  }

  def statusLine(xs: Seq[CompileStatus]) = xs.iterator.map(_.toStatusIcon).mkString

  def isEmptyOrComment(s: String) = s.isEmpty || s.startsWith("//")
}

sealed trait CompileStatus {
  def lines: List[String] = this match {
    case CompileOk          => Nil
    case CompileWarn(lines) => lines
    case CompileErr(lines)  => lines
  }

  def toStatusIcon = this match {
    case CompileOk      => s"${Console.GREEN}\u2713${Console.RESET}"  // check mark     (green)
    case CompileWarn(_) => s"${Console.YELLOW}\u2623${Console.RESET}" // biohazard sign (yellow)
    case CompileErr(_)  => s"${Console.RED}\u2717${Console.RESET}"    // cross mark     (red)
  }

  def toStatusPadded = this match {
    case CompileOk      => "ok   "
    case CompileWarn(_) => "warn "
    case CompileErr(_)  => "error"
  }
}
      case object CompileOk                                     extends CompileStatus
final case class  CompileWarn(override val lines: List[String]) extends CompileStatus
final case class  CompileErr(override val lines: List[String])  extends CompileStatus

sealed trait TestFile { def src: Path }
final case class RealTestFile(src: Path) extends TestFile
final case class InMemoryTestFile(
    src: Path,
    outerPrelude: List[List[Defn]],
    innerPrelude: List[List[Defn]],
    testStats: List[List[Stat]],
) extends TestFile

trait MkInMemoryTestFile {
  def path: Path
  def outerPrelude: List[List[Defn]]
  def innerPrelude: List[List[Defn]]
  def testStats: List[List[Stat]]

  def testFile = InMemoryTestFile(path, outerPrelude, innerPrelude, testStats)
}

sealed abstract class CompileFile(src: Path) {
  val name    = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
  def name2: String

  def writeLines(xs: List[String]) = {
    val dir     = src.resolveSibling(name)
    val chkPath = dir.resolve(s"$name2.check")
    Files.createDirectories(dir)
    xs match {
      case init :+ last => Files.writeString(chkPath, (init :+ last.stripLineEnd :+ "").mkString("\n"))
      case _            => Files.writeString(chkPath, "")
    }
  }
}

final case class CompileFile1(src: Path, id: String) extends CompileFile(src) {
  val name2 = s"$name.$id"
  val src2  = Main.targetDir.resolve(src)
}

final case class CompileFileLine(src: Path, idxInt: Int) extends CompileFile(src) {
  val idx   = if (idxInt < 10) s"0$idxInt" else s"$idxInt"
  val name2 = s"$name.$idx"
  val src2  = Main.targetDir.resolve(src).resolveSibling(s"$name2.scala")
}
