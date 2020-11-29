package shisa

import java.io.File
import java.net.URLClassLoader

import coursier._

object Deps {
  private val _2_13_head = "2.13.4" // scala -2.13.head -version
  private val _3_00_base = "3.0.0-M2"

  lazy val scalac_2_13 = fetch(scala2Art("scala-compiler",  _2_13_head))
  lazy val scalac_3_00 = fetch(scala3Art("scala3-compiler", _3_00_base))

  private val scalaEaRepo = mvn"https://scala-ci.typesafe.com/artifactory/scala-integration"
  private val scalaPrRepo = mvn"https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots"

  private def scala2Art(n: String, v: String) = Dependency(Module(org"org.scala-lang", ModuleName(n)), v)
  private def scala3Art(n: String, v: String) = Dependency(Module(org"org.scala-lang", ModuleName(s"${n}_$v")), v)

  private def fetch(dep: Dependency, repos: Repository*) = Fetch().addDependencies(dep).addRepositories(repos: _*).run()

  private val dotcCp = BuildInfo.scalac3Dir +: Deps.scalac_3_00
  private val dotcCl = new URLClassLoader(dotcCp.map(_.toURI.toURL).toArray, getClass.getClassLoader)
  private val freshCompiler3Cls  = dotcCl.loadClass("shisa.FreshCompiler3")
  private val freshCompiler3Ctor = freshCompiler3Cls.getConstructor(classOf[String], classOf[Array[File]], classOf[String])

  def FreshCompiler3(id: String, cmd: String): MkCompiler =
    freshCompiler3Ctor.newInstance(id, Deps.scalac_3_00.toArray, cmd).asInstanceOf[MkCompiler]
}
