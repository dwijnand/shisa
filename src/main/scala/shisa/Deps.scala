package shisa

import coursier._

object Deps {
  lazy val lib_2_13_base = fetch(scala2Art("library", "2.13.3"))
  lazy val lib_2_13_head = fetch(scala2Art("library", _2_13_head), scalaEaRepo)

  lazy val scalac_2_13_base = fetch(scala2Art("compiler", "2.13.3"))
  lazy val scalac_2_13_head = fetch(scala2Art("compiler", _2_13_head), scalaEaRepo)
  lazy val scalac_3_00_base = fetch(scala3Art("compiler", _3_00_base))

  def _2_13_base = "2.13.3"
  def _3_00_base = "0.26.0"

  lazy val _2_13_head = getVersion() match {
    case Exec.Result(0, List(s)) => s
    case _                       => getVersion() match { // if we get extra lines from Coursier, run again
      case Exec.Result(0, List(s)) => s
      case Exec.Result(err, lines) => sys.error(s"Fail: $err, lines:${indentedStr(lines)}")
    }
  }

  def scalaEaRepo = MavenRepository("https://scala-ci.typesafe.com/artifactory/scala-integration")
  def scalaPrRepo = MavenRepository("https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots")
  def scala2Org   = Organization("org.scala-lang")
  def scala3Org   = Organization("ch.epfl.lamp")

  def scala2Art(n: String, v: String) = gav(scala2Org, ModuleName(s"scala-$n"), v)
  def scala3Art(n: String, v: String) = gav(scala3Org, ModuleName(s"dotty-${n}_${dottyBv(v)}"), v)

  def dottyBv(v: String) = v.split("\\.").take(2).mkString(".")

  def gav(g: Organization, a: ModuleName, v: String) = Dependency(Module(g, a), v)
  def fetch(dep: Dependency, repos: Repository*) = Fetch().addDependencies(dep).addRepositories(repos: _*).run()

  def getObj[A](cls: Class[_]) = cls.getField("MODULE$").get(null).asInstanceOf[A]

  final val dq     = '"'
  val findVersion  = s"scala.util.Properties.scalaPropOrNone(${dq}maven.version.number$dq).get"
  def getVersion() = Exec.execStr(s"scala -2.13.head -e 'println($findVersion)'")
  def indentedStr(xs: Seq[Any]) = xs.iterator.map("\n  " + _).mkString
}
