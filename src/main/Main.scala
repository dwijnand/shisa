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
import scala.util.control.Exception

object Main {
  val cwdAbs = Paths.get("").toAbsolutePath

  val dotcCp = BuildInfo.scalac3Dir +: Deps.scalac_3_00_base
  val dotcCl = new URLClassLoader(dotcCp.map(_.toURI.toURL).toArray, getClass.getClassLoader)
  val freshCompiler3Cls  = dotcCl.loadClass("shisa.FreshCompiler3")
  val freshCompiler3Ctor = freshCompiler3Cls.getConstructor(classOf[String], classOf[Array[File]], classOf[String])

  def FreshCompiler3(id: String, cmd: String): Invoke =
    freshCompiler3Ctor.newInstance(id, Deps.scalac_3_00_base.toArray, cmd).asInstanceOf[Invoke]

  val combinations = Seq[Invoke](
    FreshCompiler2("2.13-base", Deps.scalac_2_13_base, ""),
    FreshCompiler2("2.13-head", Deps.scalac_2_13_head, ""),
    FreshCompiler2("2.13-new",  Deps.scalac_2_13_head, "-Xsource:3"),
    FreshCompiler3("3.0-old",                          "-source 3.0-migration"),
    FreshCompiler3("3.0",                              ""), // assumes -source 3.0 is the default
    FreshCompiler3("3.1-migr",                         "-source 3.1-migration"),
    FreshCompiler3("3.1",                              "-source 3.1"),
  )

  def main(args: Array[String]): Unit = {
    val sourceFiles = args.toList match {
      case Nil =>
        val fs = Files.find(Paths.get("tests"), /* maxDepth = */ 10, (p, _) => s"$p".endsWith(".scala")).toScala(List).sorted
        fs.tap(fs => println(s"Files: ${fs.mkString("[", ", ", "]")}"))
      case xs => xs.map(Paths.get(_)).map(p => if (p.isAbsolute) cwdAbs.relativize(p) else p)
    }

    val missing = sourceFiles.filter(!Files.exists(_))
    if (missing.nonEmpty)
      sys.error(s"Missing source files: ${missing.mkString("[", ", ", "]")}")

    if (Files.exists(Paths.get("target/tests")))
      IOUtil.deleteRecursive(Paths.get("target/tests"))

    val pool    = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    val futures = sourceFiles.map(f => pool.submit[Unit](() => compile1(f, combinations)))
    pool.shutdown()

    def abort(e: Throwable) = {
      e match {
        case null                    => println("Thread pool timeout elapsed before all tests were complete!")
        case _: InterruptedException => println("Thread pool was interrupted")
        case _                       => println("Unexpected failure")
      }

      if (e != null)
        e.printStackTrace()

      pool.shutdownNow()

      for {
        f <- futures
        _ <- allCatcher.opt(f.get(0, NANOSECONDS))
      } ()
    }

    try {
      if (!pool.awaitTermination(10, MINUTES))
        abort(e = null)
    } catch {
      case t: Throwable => abort(t)
    }
  }

  def compile1(sourceFile: Path, combinations: Seq[Invoke]) = {
    val residentCompilers = combinations.map(new CachedInvoke(_))

    if (sourceFile.toString.endsWith(".lines.scala")) {
      doCompileLines(sourceFile, combinations)
    } else {
      val results = residentCompilers.map(doCompile(sourceFile, _))
      val lines = Files.readString(sourceFile).linesIterator.size
      println(f"> ${s"$sourceFile ($lines lines)"}%-45s ${statusLine(results)}")
    }
  }

  def doCompile(sourceFile: Path, invoke: Invoke) = {
    if (Thread.interrupted()) throw new InterruptedException
    val file = CompileFile1(sourceFile, invoke.id)
    val res = invoke.compile1(file.src)
    val writeBody = s"// exitCode: ${res.exitCode}" +: res.lines.asScala
    (writeBody.init :+ writeBody.last.stripLineEnd).foreach(file.chk.println)
    file.chk.close()
    res
  }

  def doCompileLines(sourceFile: Path, combinations: Seq[Invoke]) = {
    val re   = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r
    val (setup, base, input) = Files.readString(sourceFile) match {
      case re(setup, base, cases) => (setup, base, cases.linesIterator.toList)
    }

    input.iterator.zipWithIndex.filter(!_._1.trim.pipe(isEmptyOrComment)).foreach { case (line, _idx) =>
      if (Thread.interrupted()) throw new InterruptedException
      val file = CompileFileLine(sourceFile, _idx)

      val body = Array.fill(input.size)("")
      body(_idx) = line
      Files.writeString(file.src2, s"package p${file.idx}\n\n$setup\nclass Test $base{\n${body.mkString("\n")}\n}\n")

      val results = combinations.map(invoke => (invoke.id, invoke.compile1(file.src2)))

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

  val allCatcher: Exception.Catch[Nothing] = Exception.catchingPromiscuously(Exception.allCatcher)
}

sealed abstract class CompileFile(val src: Path) {
  val name          = src.getFileName.toString.stripSuffix(".scala").stripSuffix(".lines")
  val dir           = IOUtil.createDirs(src.resolveSibling(name))
  def chkPath: Path
  lazy val chk      = new PrintWriter(Files.newBufferedWriter(chkPath), true)
}

final case class CompileFile1(_src: Path, id: String) extends CompileFile(_src) {
  val chkPath = dir.resolve(s"$name.$id.check")
}

final case class CompileFileLine(_src: Path, _idx: Int) extends CompileFile(_src) {
  val idx     = if (_idx < 10) s"0${_idx}" else s"${_idx}"
  val src2    = IOUtil.createDirs(Paths.get("target").resolve(dir)).resolve(s"$name.$idx.scala")
  val chkPath = dir.resolve(s"$name.$idx.check")
}
