package shisa

import coursier._

object Deps {
  val _2_13_head = "2.13.4" // scala -2.13.head -version
  val _3_00_base = "3.0.0-M2"

  lazy val scalac_2_13 = fetch(scala2Art("scala-compiler",  _2_13_head))
  lazy val scalac_3_00 = fetch(scala3Art("scala3-compiler", _3_00_base))

  val scalaEaRepo = mvn"https://scala-ci.typesafe.com/artifactory/scala-integration"
  val scalaPrRepo = mvn"https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots"

  def scala2Art(n: String, v: String) = Dependency(Module(org"org.scala-lang", ModuleName(n)), v)
  def scala3Art(n: String, v: String) = Dependency(Module(org"org.scala-lang", ModuleName(s"${n}_$v")), v)

  def fetch(dep: Dependency, repos: Repository*) = Fetch().addDependencies(dep).addRepositories(repos: _*).run()
}
