inThisBuild(Def.settings(
  organization := "com.dwijnand",
       version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq(scalaVersion.value, "0.26.0"),

  Global / sourcesInBase := false,
  sourceDirectory        := baseDirectory.value / "src",
  target                 := baseDirectory.value / "target",
  historyPath            := Some(target.value / ".history"),
))

val shisaRoot = proj1(project).in(file(".")).settings(baseDirectory := target.value)
aggregateProjects(shisaMain)

def compiler3Dep = Def.setting(scalaOrganization.value %% "dotty-compiler" % scalaVersion.value)
def compiler2Dep = Def.setting(scalaOrganization.value  % "scala-compiler" % scalaVersion.value)

lazy val shisaMain = proj(project).settings(
  Compile / unmanagedSourceDirectories += (
    (ThisBuild / sourceDirectory).value / (if (isDotty.value) "scalac3" else "scalac2")),
  libraryDependencies += ("io.get-coursier" %% "coursier" % "2.0.0-RC6-25").withDottyCompat(scalaVersion.value),
  libraryDependencies += (if (isDotty.value) compiler3Dep.value else compiler2Dep.value),
  Compile / mainClass := Some(if (isDotty.value) "shisa.Scala3Main" else "shisa.Scala2Main"),
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
