package shisa

import java.io.File
import java.net.URLClassLoader

import coursier._

object Deps {
  lazy val scalac_2_13 = fetch(dep"org.scala-lang:scala-compiler:2.13.4")
  lazy val scalac_3_00 = fetch(dep"org.scala-lang:scala3-compiler_3.0.0-M2:3.0.0-M2")

  val scalaEaRepo = mvn"https://scala-ci.typesafe.com/artifactory/scala-integration"
  val scalaPrRepo = mvn"https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots"

  private def fetch(dep: Dependency, repos: Repository*) = Fetch().addDependencies(dep).addRepositories(repos: _*).run()

  private val scalac3Dir         = new File("target/shisaScalac3/scala-3.0.0-M2/classes")
  private val dotcCp             = scalac3Dir +: scalac_3_00
  private val dotcCl             = new URLClassLoader(dotcCp.map(_.toURI.toURL).toArray, getClass.getClassLoader)
  private val freshCompiler3Cls  = dotcCl.loadClass("shisa.FreshCompiler3")
  private val freshCompiler3Ctor = freshCompiler3Cls.getConstructor(classOf[String], classOf[Array[File]], classOf[String])

  def FreshCompiler3(id: String, cmd: String): MkCompiler =
    freshCompiler3Ctor.newInstance(id, scalac_3_00.toArray, cmd).asInstanceOf[MkCompiler]
}
