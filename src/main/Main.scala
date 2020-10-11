package shisa

import scala.language.implicitConversions

import java.io.File
import java.net.URLClassLoader
import java.nio.file._
import java.util.concurrent.Executors

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.chaining._

import cats.data.Chain

import scala.meta._, contrib._

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

  val inMemoryTestFiles    = List(Call.hashHash, Call.pos).map(_.testFile)
  val inMemoryTestFilesMap = inMemoryTestFiles.map(tf => tf.src -> tf).toMap

  def main(args: Array[String]): Unit = {
    val testFiles = args.toList match {
      case Nil =>
        val realTestFiles = Files.find(Paths.get("testdata"), 10, (p, _) => s"$p".endsWith(".scala")).toScala(List)
          .map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
          .map(TestFile(_, None))
        (realTestFiles ::: inMemoryTestFiles).sortBy(_.src)
          .tap(fs => println(s"Files: ${fs.map(_.src).mkString("[", ", ", "]")}"))
      case xs  => xs
        .map(Paths.get(_))
        .map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
        .map(p => inMemoryTestFilesMap.getOrElse(p, TestFile(p, None)))
    }

    val missing = testFiles.collect { case TestFile(p, None) if !Files.exists(p) => p }
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

  val TestRegex = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r

  def compile1(testFile: TestFile, mkCompilers: Seq[MkCompiler]) = {
    val srcFile   = testFile.src
    val compilers = mkCompilers.map(_.mkCompiler())

    if (srcFile.toString.endsWith(".lines.scala")) {
      val (setup, base, cases) = Files.readString(srcFile) match {
        case TestRegex(setup, base, cases) => (setup, base, cases.linesIterator.toList)
      }

      cases.iterator.filter(!_.trim.pipe(isEmptyOrComment)).zipWithIndex.foreach { case (line, idx) =>
        val file        = CompileFileLine(srcFile, idx)
        val results     = doCompileLine(file, compilers, setup, base, cases.size, line)
        val lineNo      = setup.linesIterator.size + 2 + idx
        val statusIcons = results.map(_.toStatusIcon).mkString
        println(f"> ${s"$srcFile:$lineNo"}%-45s $statusIcons$line%-100s")
      }
    } else {
      val src2 = targetDir.resolve(srcFile)
      Files.createDirectories(src2.getParent)
      testFile match {
        case TestFile(_, Some(cnts)) => Files.writeString(src2, ShisaMeta.testFileSource(cnts))
        case _                       => Files.copy(srcFile, src2)
      }
      val results = compilers.map { compiler =>
        val file = CompileFile1(srcFile, compiler.id)
        val res  = compiler.compile1(src2)
        file.writeLines((s"// hasErrors: ${res.hasErrors}" +: res.lines.asScala).toList)
        res
      }
      val lines = Files.readString(src2).linesIterator.size
      println(f"> ${s"$srcFile ($lines lines)"}%-45s ${results.map(_.toStatus.toStatusIcon).mkString}")
    }
  }

  def doCompileLine(file: CompileFileLine, compilers: Seq[Compiler],
      setup: String, base: String, count: Int, line: String) = {
    val body = List.fill(file.idxInt)("") ::: line :: List.fill(count - file.idxInt - 1)("")
    val code = List(s"package p${file.idx}", "", setup, s"class Test $base{") ::: body ::: List("}", "")

    Files.createDirectories(file.src2.getParent)
    Files.writeString(file.src2, code.mkString("\n"))

    val resultsWithId = compilers.map(compiler => (compiler.id, compiler.compile1(file.src2).toStatus))
    val results       = resultsWithId.map(_._2)

    val initialRes = (CompileWarn(Nil): CompileStatus, Chain.empty[String])
    val (_, lines) = resultsWithId.foldLeft(initialRes) { case ((prevRes, lines), (id, res)) =>
      val resIdStatus = s"// ${id.padTo(9, ' ')} ${res.toStatusPadded}"
      val resLines = if (res.lines.isEmpty) Chain.empty else Chain.fromSeq(res.lines) :+ ""
      val newLines = if (res == prevRes) Chain.one(s"$resIdStatus <no change>") else resIdStatus.trim +: resLines
      (res, lines ++ newLines)
    }

    val statusSummary = results.map(_.toStatusPadded).mkString(" ").trim
    file.writeLines((s"// src: $line" +: lines :+ "" :+ statusSummary).toList)
    results
  }

  implicit class CompileResultOps(private val res: CompileResult) extends AnyVal {
    def toStatus: CompileStatus = (res.hasErrors, res.lines.asScala.toList) match {
      case (false, Nil)   => CompileOk
      case (false, lines) => CompileWarn(lines)
      case (true,  lines) => CompileErr(lines)
    }
  }

  def isEmptyOrComment(s: String) = s.isEmpty || s.startsWith("//")
}

final case class TestContents(
    outerDefns: List[List[Defn]],
    innerDefns: List[Defn],
    testStats: List[List[Stat]],
) {
  def ++(that: TestContents) = {
    TestContents(
      (outerDefns ++ that.outerDefns).distinct,
      (innerDefns ++ that.innerDefns).distinct,
      (testStats ++ that.testStats).distinct,
    )
  }
}

final case class TestFile(src: Path, contents: Option[TestContents])

trait MkInMemoryTestFile {
  def path: Path
  def outerDefns: List[List[Defn]]
  def innerDefns: List[Defn]
  def testStats: List[List[Stat]]

  def contents = TestContents(outerDefns, innerDefns, testStats)
  def testFile = TestFile(path, Some(contents))
}

sealed abstract class CompileFile(src: Path) {
  val name = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
  def name2: String

  def writeLines(xs: List[String]) = {
    val chk = src.resolveSibling(name).resolve(s"$name2.check")
    Files.createDirectories(chk.getParent)
    xs match {
      case init :+ last => Files.writeString(chk, (init :+ last.stripLineEnd :+ "").mkString("\n"))
      case _            => Files.writeString(chk, "")
    }
  }
}

final case class CompileFile1(src: Path, id: String) extends CompileFile(src) {
  val name2 = s"$name.$id"
}

final case class CompileFileLine(src: Path, idxInt: Int) extends CompileFile(src) {
  val idx   = if (idxInt < 10) s"0$idxInt" else s"$idxInt"
  val name2 = s"$name.$idx"
  val src2  = Main.targetDir.resolve(src).resolveSibling(s"$name2.scala")
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
