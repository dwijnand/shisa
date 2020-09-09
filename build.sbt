val scala2 = "2.13.4-bin-965ab82" // Early access to TASTy unpickling integration
val scala3 = "0.23.0" // need to go back for the TASTy unpickler in 2.13.x..
//val scala3 = "0.26.0"

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

val shisaRoot = proj1(project).in(file(".")).settings(baseDirectory := target.value)
aggregateProjects(shisaMain, shisaScalacI, shisaScalac2, shisaScalac3)

lazy val shisaMain = proj(project).dependsOn(shisaScalacI, shisaScalac2, shisaScalac3).settings(
)

lazy val shisaScalacI = proj(project).settings(
  libraryDependencies += "io.get-coursier" %% "coursier" % "2.0.0-RC6-25",
)

lazy val shisaScalac2 = proj(project).dependsOn(shisaScalacI).settings(
  libraryDependencies += scalaOrganization.value  % "scala-compiler" % scalaVersion.value,
)

lazy val shisaScalac3 = proj(project).dependsOn(shisaScalacI).settings(
         scalaVersion := scala3,
         crossVersion := CrossVersion.constant("2.13"), // not "0.26" or similar
  libraryDependencies += scalaOrganization.value %% "dotty-compiler" % scalaVersion.value,
  projectDependencies := projectDependencies.value.map(_.withDottyCompat(scalaVersion.value)),
)

def projId(p: Project) = uncapitalize(p.id.stripPrefix("shisa"))
def proj1(p: Project) = p.settings(
  name        := "shisa-" + projId(p), // shisaFoo => shisa-foo
  target      := (ThisBuild / target).value / thisProject.value.id, // target = /target/{id}
  historyPath := (ThisBuild / historyPath).value, // all projects share the same history file
)
def proj(p: Project) = proj1(p).in(file("src") / projId(p)).settings(
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
  Compile / unmanagedSourceDirectories += (Compile / scalaSource).value,

  Compile / javaSource        := target.value / "src/main/java",
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
