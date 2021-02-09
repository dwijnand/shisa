package shisa

import java.io.File
import java.net.URLClassLoader

import coursier._

object Deps {
  val scalaEaRepo            = mvn"https://scala-ci.typesafe.com/artifactory/scala-integration"
  val scalaPrRepo            = mvn"https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots"
  val scalac2Jars            = fetch(dep"org.scala-lang:scala-compiler:2.13.4")
  val scalac3Jars            = fetch(dep"org.scala-lang:scala3-compiler_3.0.0-M3:3.0.0-M3")
  def fetch(dep: Dependency) = Fetch().addRepositories(scalaEaRepo, scalaPrRepo).addDependencies(dep).run()
  val shisaScalac3Classes    = new File(s"target/shisaScalac3/scala-3.0.0-M3/classes")
  val scalac3Cp              = (shisaScalac3Classes +: scalac3Jars).map(_.toURI.toURL).toArray
  val scalac3Cl              = new URLClassLoader(scalac3Cp, getClass.getClassLoader)
  val freshCompiler3Class    = scalac3Cl.loadClass("shisa.FreshCompiler3")
  val freshCompiler3Ctor     = freshCompiler3Class.getConstructor(classOf[String], classOf[Array[File]], classOf[String])

  def mkScalac2(id: String, cmd: String = ""): MkCompiler =
    FreshCompiler2(id, scalac2Jars, cmd)

  def mkScalac3(id: String, cmd: String = ""): MkCompiler =
    freshCompiler3Ctor.newInstance(id, scalac3Jars.toArray, cmd).asInstanceOf[MkCompiler]
}
