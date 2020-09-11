val scala2 = "2.13.3"
val scala3 = "0.26.0"

inThisBuild(Def.settings(
  organization := "com.dwijnand",
       version := "0.1.0-SNAPSHOT",
     resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/",
  scalaVersion := scala2,

  Global / sourcesInBase := false,
  sourceDirectory        := baseDirectory.value / "src",
  target                 := baseDirectory.value / "target",
  historyPath            := Some(target.value / ".history"),
))

val shisa = proj1(project).in(file(".")).settings(sourceDirectory := target.value / "src")
aggregateProjects(shisaMain, shisaScalacI, shisaScalac2, shisaScalac3)
run := (shisaMain / Compile / run).evaluated

lazy val shisaMain = proj(project).dependsOn(shisaScalacI, shisaScalac2).settings(
  buildInfoPackage := "shisa",
  buildInfoKeys := Seq[BuildInfoKey](
    classesDir(shisaScalac3, Compile).taskValue.named("scalac3Dir"),
  ),
  libraryDependencies += "io.get-coursier" %% "coursier" % "2.0.0-RC6-25",
).enablePlugins(BuildInfoPlugin)

lazy val shisaScalacI = proj(project).settings(
  autoScalaLibrary := false,
      compileOrder := CompileOrder.JavaThenScala,
        crossPaths := false,
)

lazy val shisaScalac2 = proj(project).dependsOn(shisaScalacI).settings(
  libraryDependencies += scalaOrganization.value  % "scala-compiler" % scalaVersion.value,
)

lazy val shisaScalac3 = proj(project).dependsOn(shisaScalacI).settings(
         scalaVersion := scala3,
  libraryDependencies += scalaOrganization.value %% "dotty-compiler" % scalaVersion.value,
)

def proj1(p: Project) = p.settings(
  target      := (ThisBuild / target).value / thisProject.value.id, // target = /target/{id}
  historyPath := (ThisBuild / historyPath).value, // all projects share the same history file
)
def proj(p: Project) = proj1(p).in(file("src") / uncapitalize(p.id.stripPrefix("shisa"))).settings(
  name := "shisa-" + baseDirectory.value.getName, // src/foo => shisa-foo

  // IntelliJ's project import works better when these are set correctly.
  Seq(Compile, Test).flatMap(inConfig(_)(Seq(
    managedSourceDirectories := Nil,
    managedResourceDirectories := Nil,
    unmanagedSourceDirectories := Nil,
    unmanagedResourceDirectories := Nil,
  ))),

            sourceDirectory            := baseDirectory.value,
  Compile / sourceDirectory            := sourceDirectory.value,
  Compile / scalaSource                := (Compile / sourceDirectory).value,
  Compile / javaSource                 := (Compile / sourceDirectory).value,
  Compile / unmanagedSourceDirectories += (Compile / scalaSource).value,
  Compile / unmanagedSourceDirectories += (Compile / javaSource).value,

  Compile / resourceDirectory := target.value / "src/main/resources",
     Test / sourceDirectory   := target.value / "src/test",
)

def uncapitalize(s: String) = {
  if (s == null) null
  else if (s.length == 0) ""
  else if (s.charAt(0).isLower) toString
  else {
    val chars = s.toCharArray
    chars(0) = chars(0).toLower
    new String(chars)
  }
}

def classesDir(r: Reference, c: ConfigKey) = (r / c / classDirectory).toTask.dependsOn(r / c / compile)
