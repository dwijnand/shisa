val scalaV2 = "2.13.3"
val scalaV3 = "0.26.0"

inThisBuild(Def.settings(
  organization := "com.dwijnand",
       version := "0.1.0-SNAPSHOT",
  scalaVersion := scalaV3,

  Global / sourcesInBase := false,
  sourceDirectory        := baseDirectory.value / "src",
  target                 := baseDirectory.value / "target",
  historyPath            := Some(target.value / ".history"),
  scalacOptions          += "-language:implicitConversions",
))

val shisa = proj1(project).in(file(".")).settings(sourceDirectory := target.value / "src")
aggregateProjects(shisaMain)
run := (shisaMain / Compile / run).evaluated

lazy val shisaMain = proj(project).settings(
  libraryDependencies += "io.get-coursier" %% "coursier" % "2.0.0-RC6-25" withDottyCompat scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaV2,
  libraryDependencies += "ch.epfl.lamp"  %% "dotty-compiler" % scalaV3,
  Compile / mainClass := Some("shisa.Main"),
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
