package shisa

import coursier._

object Deps {
  val _2_13_base = "2.13.3"
  val _2_13_head = "2.13.4-bin-8b9858e" // scala -2.13.head -version
  val _3_00_base = "0.27.0-RC1"

  lazy val scalac_2_13_base = fetch(scala2Art("scala-compiler", _2_13_base))
  lazy val scalac_2_13_head = fetch(scala2Art("scala-compiler", _2_13_head), scalaEaRepo)
  lazy val scalac_3_00_base = fetch(scala3Art("dotty-compiler", _3_00_base))

  val scalaEaRepo = MavenRepository("https://scala-ci.typesafe.com/artifactory/scala-integration")
  val scalaPrRepo = MavenRepository("https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots")
  val scala2Org   = Organization("org.scala-lang")
  val scala3Org   = Organization("ch.epfl.lamp")

  def scala2Art(n: String, v: String) = Dependency(Module(scala2Org, ModuleName(n)), v)
  def scala3Art(n: String, v: String) = Dependency(Module(scala3Org, ModuleName(s"${n}_${dottyBv(v)}")), v)
  def dottyBv(v: String) = v.split("\\.").take(2).mkString(".")

  def fetch(dep: Dependency, repos: Repository*) = Fetch().addDependencies(dep).addRepositories(repos: _*).run()
}
