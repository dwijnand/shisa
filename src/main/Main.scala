package shisa

import scala.language.implicitConversions

import java.io.{ File, PrintWriter }
import java.net.URLClassLoader
import java.nio.file._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import scala.util.chaining._

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

    println(s"Combinations:${combinations.map(i => f"\n  ${i.id}%-9s : ${i.cmd}").mkString}")

    sourceFiles.foreach(compile1(_, combinations))
  }

  def compile1(sourceFile: Path, combinations: Seq[Invoke]) = {
    val residentCompilers = combinations.map(new CachedInvoke(_))

    print(f"> Testing $sourceFile%-50s")
    if (sourceFile.toString.endsWith(".lines.scala")) {
      println()
      doCompileLines(sourceFile, combinations)
    } else {
      print(" ")
      residentCompilers.foreach(doCompile(sourceFile, _))
      println()
    }
  }

  def doCompile(sourceFile: Path, invoke: Invoke) = {
    if (Thread.interrupted()) throw new InterruptedException
    val file = CompileFile1(sourceFile, invoke.id)
    val res = invoke.compile1(file.src)
    val writeBody = s"// exitCode: ${res.exitCode}" +: res.lines.asScala
    (writeBody.init :+ writeBody.last.stripLineEnd).foreach(file.chk.println)
    file.chk.close()
    printStatus(res)
  }

  def doCompileLines(sourceFile: Path, combinations: Seq[Invoke]) = {
    val re   = """(?s)(.*)class Test ([^{]*)\{\n(.*)\n}\n""".r
    val (setup, base, input) = Files.readString(sourceFile) match {
      case re(setup, base, cases) => (setup, base, cases.linesIterator.toList)
    }

    def emptyOrCommented(s: String) = s.isEmpty || s.startsWith("//")
    input.iterator.zipWithIndex.filter(!_._1.trim.pipe(emptyOrCommented)).foreach { case (line, _idx) =>
      if (Thread.interrupted()) throw new InterruptedException
      val file = CompileFileLine(sourceFile, _idx)
      print(f"    line ${file.idx} $line%-100s ")

      val body = Array.fill(input.size)("")
      body(_idx) = line
      Files.writeString(file.src2, s"package p${file.idx}\n\n$setup\nclass Test $base{\n${body.mkString("\n")}\n}\n")

      val results = combinations.map(invoke => (invoke.id, invoke.compile1(file.src2).tap(printStatus)))

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
      println()
    }
  }

  def printStatus(res: CompileResult) = (res.exitCode, res.lines.asScala.toList) match {
    case (0, Nil) => print(pass)
    case (0, _)   => print(warn)
    case (_, _)   => print(fail)
  }

  def fail = s"${Console.RED}\u2717${Console.RESET}"    // cross mark (red)
  def pass = s"${Console.GREEN}\u2713${Console.RESET}"  // check mark (green)
  def warn = s"${Console.YELLOW}\u2623${Console.RESET}" // biohazard sign (yellow)

  def statusPadded(res: CompileResult) = (res.exitCode, res.lines.asScala.toList) match {
    case (0, Nil) => "ok   "
    case (0, _)   => "warn "
    case (_, _)   => "error"
  }
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

case class TimeoutException(duration: Duration) extends RuntimeException
