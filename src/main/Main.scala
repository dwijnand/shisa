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

  val mkCompilers = List[MkCompiler](
    FreshCompiler2("2.13-base", Deps.scalac_2_13_base, ""),
    FreshCompiler2("2.13-head", Deps.scalac_2_13_head, ""),
    FreshCompiler2("2.13-new",  Deps.scalac_2_13_head, "-Xsource:3"),
    FreshCompiler3("3.0-old",                          "-source 3.0-migration"),
    FreshCompiler3("3.0",                              ""), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr",                         "-source 3.1-migration"),
    FreshCompiler3("3.1",                              "-source 3.1"),
  )

  val inMemoryMkTest = List(
    Call.hashHash,
    Call.pos,
    Call.def_meth_p,
    Call.def_prop_m,
    Call.switch_m2p_m,
    Call.switch_m2p_p,
    Call.switch_p2m_m,
    Call.switch_p2m_p,
    Call.switch_vc_m2p_m,
    Call.switch_vc_m2p_p,
    Call.switch_vc_p2m_m,
    Call.switch_vc_p2m_p,
    EtaX.boom,
    EtaX.meth2,
    EtaX.cloneEta,
    EtaX.methF0,
    EtaX.prop,
  )
  val inMemoryTests  = inMemoryMkTest.map(mk => TestFile(mk.path, Some(mk.contents)))

  def main(args: Array[String]): Unit = {
    val testFiles = args.toList match {
      case Nil =>
        val realTestFiles = Files.find(Paths.get("testdata"), 10, (p, _) => s"$p".endsWith(".scala")).toScala(List)
          .map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
          .map(TestFile(_, None))
        (realTestFiles ::: inMemoryTests).sortBy(_.src)
          .tap(fs => println(s"Files: ${fs.map(_.src).mkString("[", ", ", "]")}"))
      case xs  => xs
        .map(Paths.get(_))
        .map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
        .map(p => inMemoryTests.find(_.src == p).getOrElse(TestFile(p, None)))
    }

    testFiles.collect { case TestFile(p, None) if !Files.exists(p) => p } match {
      case Nil     =>
      case missing => sys.error(s"Missing test files: ${missing.mkString("[", ", ", "]")}")
    }

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
      case TestFile(src, _) if src.toString.endsWith(".lines.scala") => doLines(testFile, compilers)
      case TestFile(src, Some(contents))                             => doUnit(src, contents, compilers)
      case _                                                         => throw new Exception(s"Expected lines or TestContents: $testFile")
    }
  }

  final case class CompileFileLine(src: Path, idxInt: Int) {
    val name  = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
    val idx   = if (idxInt < 10) s"0$idxInt" else s"$idxInt"
    val name2 = s"$name.$idx"
    val src2  = Main.targetDir.resolve(src).resolveSibling(s"$name2.scala")

    def writeLines(xs: List[String]) = {
      val chk = src.resolveSibling(name).resolve(s"$name2.check")
      Files.createDirectories(chk.getParent)
      val text = xs match {
        case init :+ last => (init :+ last.stripLineEnd :+ "").mkString("\n")
        case _            => ""
      }
      Files.writeString(chk, text)
    }
  }

  val noMsg        = new Msg(Severity.Error, "nopath.scala", 1, "Mismatch zipAll", "Mismatch zipAll")
  val noRes        = new Msgs(List(noMsg).asJava)
  val noFile       = CompileFileLine(Paths.get("nopath.scala"), -1)
  val noCompilerId = "<unknown>"
  val noMsgss      = List[List[Msg]](Nil, Nil, Nil, Nil, Nil, Nil, Nil)
  val noMsgss2     = new Msgs(List(noMsg).asJava)
  val noMsgssAndId = (new Msgs(List(noMsg).asJava), noCompilerId)
  val LineStart    = "(?m)^".r
  val TestRegex    = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r

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
    for ((expMsgs, (obtMsgs, compilerId)) <- expMsgs.zipAll(msgssAndId, List(noMsg), (noRes, noCompilerId))) {
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

  def doLines(testFile: TestFile, compilers: List[Compiler]) = {
    val sourceStr = testFile match {
      case TestFile(src, None)           => Files.readString(src)
      case TestFile(  _, Some(contents)) => ShisaMeta.testFileSource(contents)
    }
    val (setup, base, cases) = sourceStr match {
      case TestRegex(setup, base, cases) => (setup, base, cases.linesIterator.toList)
    }

    val msgssAndIds = cases.iterator.filter(!_.trim.pipe(isEmptyOrComment)).zipWithIndex.map { case (line, idx) =>
      val file        = CompileFileLine(testFile.src, idx)
      val msgssAndId  = doCompileLine(file, compilers, setup, base, cases.size, line)
      val lineNo      = setup.linesIterator.size + 2 + idx
      val statusIcons = msgssAndId.map(_._1.toResult.toStatusIcon).mkString
      println(f"* ${s"${file.src}:$lineNo"}%-50s $statusIcons$line%-100s")
      msgssAndId
    }

    val zeroElem   = (new Msgs(Nil.asJava), noCompilerId)
    val zero       = List(zeroElem, zeroElem, zeroElem, zeroElem, zeroElem, zeroElem, zeroElem)
    val msgssAndId = msgssAndIds.foldLeft(zero) { (acc, msgssAndId) =>
      acc.zipAll(msgssAndId, noMsgssAndId, noMsgssAndId).map { case ((msgsA, idA0), (msgsB, idB)) =>
        val idA = if (idA0 == noCompilerId) idB else idA0
        assert(idA == idB, s"$idA != $idB")
        (new Msgs((msgsA.asList ::: msgsB.asList).asJava), idA)
      }
    }
    testFile.contents.foreach { contents =>
      compareMsgs(contents.expectedMsgs, msgssAndId, testFile.src)
    }
  }

  def doCompileLine(file: CompileFileLine, compilers: List[Compiler],
      setup: String, base: String, count: Int, line: String) = {
    val body = List.fill(file.idxInt)("") ::: line :: List.fill(count - file.idxInt - 1)("")
    Files.createDirectories(file.src2.getParent)
    Files.writeString(file.src2,
      s"""package p${file.idx}
         |
         |$setup
         |class Test $base{
         |${body.mkString("\n")}
         |}
         |""".stripMargin
    )

    val msgssAndId = compilers.map(compiler => (compiler.compile1(file.src2), compiler.id))
    val msgs0      = (noMsgss2, Chain.empty[String])
    val (_, lines) = msgssAndId.foldLeft(msgs0) { case ((prevMsgs, lines), (msgs, id)) =>
      val idStatus = s"// ${id.padTo(9, ' ')} ${msgs.toResult.toStatusPadded}"
      val resLines = if (msgs.lines.isEmpty) Chain.empty else Chain.fromSeq(msgs.lines) :+ ""
      val newLines = if (msgs == prevMsgs) Chain.one(s"$idStatus <no change>") else idStatus.trim +: resLines
      (msgs, lines ++ newLines)
    }

    val statusSummary = msgssAndId.map(_._1.toResult.toStatusPadded).mkString(" ").trim
    file.writeLines((s"// src: $line" +: lines :+ "" :+ statusSummary).toList)
    msgssAndId
  }

  def showSev(sev: Severity) = sev match {
    case Severity.Error   => "  error"
    case Severity.Warning => "warning"
    case Severity.Info    => "   info"
  }
  def showMsg(msg: Msg) = s"${msg.path}:${msg.lineNo} ${showSev(msg.severity)}: ${msg.text}"
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

final case class TestFile(src: Path, contents: Option[TestContents])

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
