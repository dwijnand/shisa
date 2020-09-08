import sbt._, Keys._

object BuildTestsPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger  = allRequirements

  object autoImport {
    val assertBaseDir = taskKey[List[String]]("")
  }
  import autoImport._

  override def globalSettings = Seq(
  )

  override def buildSettings = Seq(
  )

  override def projectSettings = Seq(
    assertBaseDir := assertBaseDirImpl.value,
  )

  def assertBaseDirImpl = Def.task {
    def check[A](l: String, x: A, r: String, y: A) = {
      if (x == y) s"OK $l == $r: $x"
      else        s"KO $l != $r: $x vs $y"
    }

    val baseDir  = baseDirectory.value
    val baseDir2 = thisProject.value.base
    val buildSrc = (ThisBuild / sourceDirectory).value

    val checks = List(
      check("baseDirectory.value", baseDir, "thisProject.value.base", baseDir2),
      check("(ThisBuild / sourceDirectory).value", buildSrc, "baseDirectory.value.getParentFile", baseDir.getParentFile),
      check("(ThisBuild / sourceDirectory).value", buildSrc, "thisProject.value.base.getParentFile", baseDir2.getParentFile),
    )
    val failures = checks.filter(_.startsWith("KO"))
    val isRoot = projectID.value == (LocalRootProject / projectID).value
    assert(failures.isEmpty || isRoot, failures)
    checks
  }
}
