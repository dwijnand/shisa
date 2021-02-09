val scalaV2 = "2.13.4"
val scalaV3 = "3.0.0-M3"

inThisBuild(Def.settings(
     organization := "com.dwijnand",
          version := "0.1.0-SNAPSHOT",
        resolvers += "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/",
     scalaVersion := scalaV2,
   scalacOptions ++= List("-deprecation", "-feature", "-unchecked", "-Xlint"),
    scalacOptions += "-language:_",
    scalacOptions += "-Wunused:-imports",
    sourcesInBase := false,
  sourceDirectory := baseDirectory.value / "src",
           target := baseDirectory.value / "target",
      historyPath := Some(target.value / ".history"),
))

val shisa = proj1(project).in(file(".")).settings(sourceDirectory := target.value / "src")
aggregateProjects(shisaScalacI, shisaScalac2, shisaScalac3, shisaMain, shisaTests)

Compile / console      := (shisaMain / Compile / console).value
Compile / consoleQuick := (shisaMain / Compile / consoleQuick).value
Compile / run          := (shisaMain / Compile / run).evaluated
Compile / runMain      := (shisaMain / Compile / runMain).evaluated
   Test / test         := (shisaTests / Test / test).value
   Test / testOnly     := (shisaTests / Test / testOnly).evaluated
   Test / testQuick    := (shisaTests / Test / testQuick).evaluated

val shisaScalacI = proj(project)

val shisaScalac2 = proj(project).dependsOn(shisaScalacI).settings(
  libraryDependencies += scalaOrganization.value  % "scala-compiler" % scalaVersion.value,
)

val shisaScalac3 = proj(project).dependsOn(shisaScalacI).settings(
         scalaVersion := scalaV3,
        scalacOptions -= "-Xlint",
        scalacOptions -= "-Wunused:-imports",
  libraryDependencies += scalaOrganization.value %% "scala3-compiler" % scalaVersion.value,
)

val shisaMain = proj(project).dependsOn(shisaScalacI, shisaScalac2).settings(
  buildInfoPackage := "shisa",
  buildInfoKeys := Seq[BuildInfoKey](
    classesDir(shisaScalac3, Compile).taskValue.named("scalac3Dir"),
  ),
  libraryDependencies ++= List(
    "org.typelevel"   %% "cats-core"         % "2.3.1",
    "io.get-coursier" %% "coursier"          % "2.0.2",
    "org.scalameta"   %% "scalameta"         % "4.4.8",
    "ch.epfl.scala"   %% "scalafix-rules"    % "0.9.25",
  //"ch.epfl.scala"   %  "scalafix-cli"      % "0.9.25" cross CrossVersion.full,
  )
).enablePlugins(BuildInfoPlugin)

val shisaTests = proj(project).in(file("tests")).dependsOn(shisaMain).settings(
  Compile / unmanagedSourceDirectories := Nil,
     Test / unmanagedSourceDirectories += (Test / sourceDirectory).value,
     Test / sourceDirectory            := sourceDirectory.value,
                   libraryDependencies += "org.scalameta" %% "munit"        % "0.7.13" % Test,
                   libraryDependencies += "qa.hedgehog"   %% "hedgehog-sbt" % "0.5.1"  % Test,
                        testFrameworks += TestFramework("munit.Framework"),
                        testFrameworks += TestFramework("hedgehog.sbt.Framework"),
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
      unmanagedSourceDirectories := Nil,
      managedResourceDirectories := Nil,
    unmanagedResourceDirectories := Nil,
  ))),

            sourceDirectory            := baseDirectory.value,
  Compile / sourceDirectory            := sourceDirectory.value,
  Compile / scalaSource                := (Compile / sourceDirectory).value,
  Compile / javaSource                 := (Compile / sourceDirectory).value,
  Compile / unmanagedSourceDirectories += (Compile / sourceDirectory).value,

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

Global / excludeLintKeys ++= Set(javaSource, resourceDirectory, scalaSource)
