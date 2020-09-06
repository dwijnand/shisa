val shisa = project in file(".")

inThisBuild(Def.settings(
  organization := "com.dwijnand",
       version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq(scalaVersion.value, "0.26.0"),
))

Compile / unmanagedSourceDirectories +=
  (Compile / sourceDirectory).value / (if (isDotty.value) "scala3" else "scala2")

def compiler3Dep = Def.setting(scalaOrganization.value %% "dotty-compiler" % scalaVersion.value)
def compiler2Dep = Def.setting(scalaOrganization.value  % "scala-compiler" % scalaVersion.value)

libraryDependencies += ("io.get-coursier" %% "coursier" % "2.0.0-RC6-25").withDottyCompat(scalaVersion.value)
libraryDependencies += (if (isDotty.value) compiler3Dep.value else compiler2Dep.value)

Compile / mainClass := Some(if (isDotty.value) "shisa.Scala3Main" else "shisa.Scala2Main")
