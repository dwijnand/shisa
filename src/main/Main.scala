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

  val mkCompilers = List[MkCompiler](
    FreshCompiler2("2.13-base", Deps.scalac_2_13_base, ""),
    FreshCompiler2("2.13-head", Deps.scalac_2_13_head, ""),
    FreshCompiler2("2.13-new",  Deps.scalac_2_13_head, "-Xsource:3"),
    FreshCompiler3("3.0-old",                          "-source 3.0-migration"),
    FreshCompiler3("3.0",                              ""), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr",                         "-source 3.1-migration"),
    FreshCompiler3("3.1",                              "-source 3.1"),
  )

  val inMemoryTests = (Call.tests ::: EtaX.tests).map(mk => TestFile(mk.path, mk.contents))

  def main(args: Array[String]): Unit = {
    val testFilePaths = args.toList match {
      case Nil => inMemoryTests
        .map(_.src)
        .sorted
        .tap(fs => println(s"Files: ${fs.mkString("[", ", ", "]")}"))
      case xs  => xs
        .map(Paths.get(_))
        .map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
    }

    testFilePaths.collect { case p if inMemoryTests.forall(_.src != p) => p } match {
      case Nil     =>
      case missing => sys.error(s"Missing test files: ${missing.mkString("[", ", ", "]")}")
    }

    val testFiles = testFilePaths.map(p => inMemoryTests.find(_.src == p).get)

    if (Files.exists(Paths.get("target/testdata")))
      IOUtil.deleteRecursive(Paths.get("target/testdata"))

    val pool    = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val futures = testFiles.map(f => pool.submit[Unit](() => compile1(f, mkCompilers)))

    pool.shutdown()

    if (!pool.awaitTermination(10, MINUTES))
      throw new Exception("Thread pool timeout elapsed before all tests were complete!")

    futures.foreach(_.get(0, NANOSECONDS))
  }

  implicit class MsgsOps(private val _msgs: Msgs) extends AnyVal {
    def asList: List[Msg]   = _msgs.msgs.asScala.toList
    def hasErrors: Boolean  = asList.exists(_.severity == Severity.Error)
    def lines: List[String] = asList.map(_.output)

    def toResult: CompileResult = (hasErrors, lines) match {
      case (false, Nil)   => CompileOk
      case (false, lines) => CompileWarn(lines)
      case (true,  lines) => CompileErr(lines)
    }
  }

  def compile1(testFile: TestFile, mkCompilers: List[MkCompiler]) = {
    val compilers = mkCompilers.map(_.mkCompiler())
    testFile match {
      case TestFile(src, contents) if src.toString.endsWith(".lines.scala") => doLines(src, contents, compilers)
      case TestFile(src, contents)                                          => doUnit(src, contents, compilers)
      case _                                                                => throw new Exception(s"Expected lines or TestContents: $testFile")
    }
  }

  final case class CompileFileLine(src: Path, idxInt: Int) {
    val name  = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
    val idx   = if (idxInt < 10) s"0$idxInt" else s"$idxInt"
    val name2 = s"$name.$idx"
    val src2  = Main.targetDir.resolve(src).resolveSibling(s"$name2.scala")
  }

  val noMsg        = new Msg(Severity.Error, "nopath.scala", 1, "Mismatch zipAll", "Mismatch zipAll")
  val noMsgs       = new Msgs(List(noMsg).asJava)
  val noCompilerId = "<unknown>"
  val noMsgssAndId = (noMsgs, noCompilerId)
  val LineStart    = "(?m)^".r

  def doUnit(src: Path, contents: TestContents, compilers: List[Compiler]) = {
    val src2 = targetDir.resolve(src)
    Files.createDirectories(src2.getParent)
    Files.writeString(src2, ShisaMeta.testFileSource(contents))
    val msgssAndId = compilers.map(compiler => compiler.compile1(src2) -> compiler.id)
    val lines = Files.readString(src2).linesIterator.size
    println(f"* ${s"$src ($lines lines)"}%-50s ${msgssAndId.map(_._1.toResult.toStatusIcon).mkString}")
    compareMsgs(contents.expectedMsgs, msgssAndId, src)
  }

  def compareMsgs(expMsgs: List[List[Msg]], msgssAndId: List[(Msgs, String)], src: Path) = {
    for ((expMsgs, (obtMsgs, compilerId)) <- expMsgs.zipAll(msgssAndId, List(noMsg), (noMsgs, noCompilerId))) {
      def showExp(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), Console.RED   + "  -") + Console.RESET
      def showObt(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), Console.GREEN + "  +") + Console.RESET
      expMsgs.zipAll(msgsDropSummary(obtMsgs), null, null).collect {
        case ( exp, null) if exp != null => showExp(exp)
        case (null,  obt) if obt != null => showObt(obt)
        case ( exp,  obt) if exp != obt  => showExp(exp) + showObt(obt)
      }.filter(_ != "").mkString match {
        case ""    =>
        case lines =>
          val str = s"$src: message mismatch (compiler $compilerId) (${Console.RED}-expected${Console.RESET}/${Console.GREEN}+obtained${Console.RESET}):$lines"
          println(str)
          throw new Exception(str)
      }
    }
  }

  def msgsDropSummary(msgs: Msgs) = {
    // drop summary ("3 errors"/"3 errors found")
    msgs.msgs.asScala.toList.takeWhile { msg =>
      (msg.path, msg.lineNo) match {
        case ("", 0)          => false // stop
        case ("<no file>", 0) => false // stop
        case _                => true  // continue
      }
    }
  }

  def doLines(src: Path, contents: TestContents, compilers: List[Compiler]) = {
    val msgssAndIds = contents.testStats.flatten.zipWithIndex.map { case (stat, idx) =>
      val file        = CompileFileLine(src, idx)
      val msgssAndId  = doCompileLine(file, compilers, contents, stat)
      val line        = stat.toString()
      val lineNo      = -1 // setup.linesIterator.size + 2 + idx
      val statusIcons = msgssAndId.map(_._1.toResult.toStatusIcon).mkString
      println(f"* ${s"${file.src}:$lineNo"}%-50s $statusIcons$line%-100s")
      msgssAndId
    }

    def z(id: String) = (new Msgs(Nil.asJava), id)
    val zero          = List(z("2.13-base"), z("2.13-head"), z("2.13-new"), z("3.0-old"), z("3.0"), z("3.1-migr"), z("3.1"))
    val msgssAndId    = msgssAndIds.foldLeft(zero) { (acc, msgssAndId) =>
      val acc2 = acc.zipAll(msgssAndId, noMsgssAndId, noMsgssAndId).map { case ((msgs, id), (newMsgs, idB)) =>
        assert(id == idB, s"$id != $idB")
        (new Msgs((msgs.asList ::: msgsDropSummary(newMsgs)).asJava), id)
      }
      acc2
    }

    compareMsgs(contents.expectedMsgs, msgssAndId, src)
  }

  def doCompileLine(file: CompileFileLine, compilers: List[Compiler], contents: TestContents, stat: Stat) = {
    val count = contents.testStats.flatten.size
    val testStats = List.fill(file.idxInt)(Nil) ::: List(stat) :: List.fill(count - file.idxInt - 1)(Nil)
    val contents1 = contents.copy(testStats = testStats)
    val sourceStr = ShisaMeta.testFileSource(contents1, Some(Term.Name(s"p${file.idx}")))
    Files.createDirectories(file.src2.getParent)
    Files.writeString(file.src2, sourceStr)
    compilers.map(compiler => (compiler.compile1(file.src2), compiler.id))
  }

  def showSev(sev: Severity) = sev match {
    case Severity.Error => "  error"
    case Severity.Warn  => "warning"
    case Severity.Info  => "   info"
  }
  def showMsg(msg: Msg) = s"${msg.path}:${msg.lineNo} ${showSev(msg.severity)}: ${msg.text.replaceAll("\n", "\\\\n")}"
  def showMsgs(msgs: List[Msg]) = msgs.iterator.map(msg => "\n  " + showMsg(msg)).mkString

  def isEmptyOrComment(s: String) = s.isEmpty || s.startsWith("//")
}

final case class TestContents(
    outerDefns: List[List[Defn]],
     baseClass: Option[Defn.Class],
    innerDefns: List[Defn],
    testStats: List[List[Stat]],
    expectedMsgs: List[List[Msg]],
) {
  def ++(that: TestContents) = {
    TestContents(
      outerDefns ::: that.outerDefns,
      mergeBaseClasses(baseClass, that.baseClass),
      innerDefns ::: that.innerDefns,
      testStats ::: that.testStats,
      expectedMsgs.zipAll(that.expectedMsgs, Nil, Nil).map { case (as, bs) => (as ::: bs).distinct },
    )
  }

  private def mergeBaseClasses[A](x: Option[A], y: Option[A]) = (x, y) match {
    case (x @ Some(_), None)          => x
    case (None, y @ Some(_))          => y
    case (None, None)                 => None
    case (Some(x), Some(y)) if x == y => None
    case (Some(x), Some(y))           => throw new Exception(s"Can't mergeBaseClasses $x ++ $y")
  }
}

final case class TestFile(src: Path, contents: TestContents)

sealed trait CompileResult {
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
      case object CompileOk                                     extends CompileResult
final case class  CompileWarn(override val lines: List[String]) extends CompileResult
final case class  CompileErr (override val lines: List[String]) extends CompileResult
