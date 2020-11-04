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
  val cwdAbs      = Paths.get("").toAbsolutePath
  val testdataDir = Paths.get("target/testdata")

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

  val compilerIds   = mkCompilers.map(_.id)
  val inMemoryTests = (Call.tests ::: EtaX.tests).map(mk => (mk.path, mk.contents))
  val NoContents    = TestContents(Nil, None, Nil, Nil, Nil)

  def makeRelative(p: Path) = if (p.isAbsolute) cwdAbs.relativize(p) else p

  def main(args: Array[String]): Unit = {
    val testFilePaths = args.toList match {
      case Nil => inMemoryTests.map(_._1).sorted.tap(fs => println(s"Files: ${fs.mkString("[", ", ", "]")}"))
      case xs  => xs.map(Paths.get(_)).map(makeRelative(_))
    }

    val testFiles = testFilePaths.map(p => inMemoryTests.find(_._1 == p).getOrElse((p, NoContents)))

    testFiles.collect { case (p, NoContents) => p } match {
      case Nil     =>
      case missing => sys.error(s"Missing test files: ${missing.mkString("[", ", ", "]")}")
    }

    if (Files.exists(testdataDir))
      IOUtil.deleteRecursive(testdataDir)

    val pool    = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val futures = testFiles.map { case (src, contents) =>
      pool.submit[Unit](() => compile1(src, contents, mkCompilers))
    }

    pool.shutdown()

    if (!pool.awaitTermination(10, MINUTES))
      throw new Exception("Thread pool timeout elapsed before all tests were complete!")

    futures.foreach(_.get(0, NANOSECONDS))
  }

  def compile1(src: Path, contents: TestContents, mkCompilers: List[MkCompiler]) = {
    val compilers = mkCompilers.map(_.mkCompiler())
    if (src.toString.endsWith(".lines.scala")) doLines(src, contents, compilers)
    else                                        doUnit(src, contents, compilers)
  }

  def doUnit(src: Path, contents: TestContents, compilers: List[Compiler]) = {
    val src2      = testdataDir.resolve(src)
    val sourceStr = ShisaMeta.testFileSource(contents)
    val msgss     = writeAndCompile(compilers, src2, sourceStr)
    compareMsgs(contents.expectedMsgs, msgss, src)
  }

  def doLines(src: Path, contents: TestContents, compilers: List[Compiler]) = {
    val msgss = contents.testStats.flatten.zipWithIndex.map { case (stat, idxInt) =>
      val name      = src.getFileName.toString.stripSuffix(".lines.scala")
      val idx       = if (idxInt < 10) s"0$idxInt" else s"$idxInt"
      val src2      = testdataDir.resolve(src).resolveSibling(s"$name.$idx.scala")
      val contents2 = contents.copy(testStats = List(List(stat)))
      val sourceStr = ShisaMeta.testFileSource(contents2, Some(Term.Name(s"p$idx")))
      writeAndCompile(compilers, src2, sourceStr)
    }.foldLeft(compilerIds.map(_ => List.empty[Msg])) { (acc, msgss) =>
      acc.zip(msgss).map { case (a, b) => a ::: b }
    }
    compareMsgs(contents.expectedMsgs, msgss, src)
  }

  def writeAndCompile(compilers: List[Compiler], src2: Path, sourceStr: String) = {
    println(s"* $src2")
    Files.createDirectories(src2.getParent)
    Files.writeString(src2, sourceStr)
    compilers.map(compiler => msgsDropSummary(compiler.compile1(src2)))
  }

  val MissingExp = new Msg(Severity.Error, "exp.scala", 0, "missing exp msg", "")
  val MissingObt = new Msg(Severity.Error, "obt.scala", 0, "missing obt msg", "")

  def compareMsgs(expMsgss: List[List[Msg]], obtMsgss: List[List[Msg]], src: Path) = {
    for (((expMsgs, obtMsgs), compilerId) <- expMsgss.zip(obtMsgss).zip(compilerIds)) {
      expMsgs.zipAll(obtMsgs, MissingExp, MissingObt).collect {
        case (exp, MissingObt)        => showExp(exp)
        case (MissingExp, obt)        => showObt(obt)
        case (exp, obt) if exp != obt => showExp(exp) + showObt(obt)
      }.mkString match {
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

  val LineStart         = "(?m)^".r
  def showExp(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), Console.RED   + "  -") + Console.RESET
  def showObt(msg: Msg) = "\n" + LineStart.replaceAllIn(showMsg(msg), Console.GREEN + "  +") + Console.RESET
  def showMsg(msg: Msg) = s"${msg.path}:${msg.lineNo} ${showSev(msg.severity)}: ${msg.text.replaceAll("\n", "\\\\n")}"

  def showSev(sev: Severity) = sev match {
    case Severity.Error => "  error"
    case Severity.Warn  => "warning"
    case Severity.Info  => "   info"
  }
}

final case class TestContents(
    outerDefns: List[List[Defn]],
     baseClass: Option[Defn.Class],
    innerDefns: List[Defn],
    testStats: List[List[Stat]],
    expectedMsgs: List[List[Msg]],
) {
  def ++(that: TestContents) = TestContents(
    outerDefns ::: that.outerDefns,
    mergeBaseClasses(baseClass, that.baseClass),
    innerDefns ::: that.innerDefns,
    testStats ::: that.testStats,
    expectedMsgs.zipAll(that.expectedMsgs, Nil, Nil).map { case (as, bs) => (as ::: bs).distinct },
  )

  private def mergeBaseClasses[A](x: Option[A], y: Option[A]) = (x, y) match {
    case (x @ Some(_), None)          => x
    case (None, y @ Some(_))          => y
    case (None, None)                 => None
    case (Some(x), Some(y)) if x == y => Some(x)
    case (Some(x), Some(y))           => throw new Exception(s"Can't mergeBaseClasses $x ++ $y")
  }
}
