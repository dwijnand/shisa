import sbt._, Keys._

object ShowPathsPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger  = allRequirements

  object autoImport {
  }
  import autoImport._

  override def globalSettings = Seq(
    commands += showPaths,
  )

  override def buildSettings = Seq(
  )

  override def projectSettings = Seq(
  )

  def showPaths: Command = Command.command("showPaths") { s =>
    val boolKeys = List(
      sourcesInBase, // If true, sources from the project's base directory are included as main sources
      crossPaths,    // If true, enables cross paths, which distinguish input and output directories for cross-building
    )

    // baseDirectory,   // The base directory.  Depending on the scope, this is the base directory for the build, project, configuration, or task

    val dirKeys = List(
      sourceDirectory,   // Default directory containing sources
      scalaSource,       // Default Scala source directory

      target,            // Main directory for files generated by the build
      javaSource,        // Default Java source directory
      resourceDirectory, // Default unmanaged resource directory, used for user-defined resources
      crossTarget,       // Main directory for files generated by the build that are cross-built
      classDirectory,    // Directory for compiled classes and copied resources
      sourceManaged,     // Default directory for sources generated by the build
      resourceManaged,   // Default managed resource directory, used when generating resources
    )

    val dirsKeys = List(
                 sourceDirectories, // List of all source directories, both managed and unmanaged
               resourceDirectories, // List of all resource directories, both managed and unmanaged
        unmanagedSourceDirectories, // Unmanaged source directories, which contain manually created sources
      unmanagedResourceDirectories, // Unmanaged resource directories, containing resources manually created by the user
          managedSourceDirectories, // Managed source directories, which contain sources generated by the build
        managedResourceDirectories, // List of managed resource directories
    )

    //cleanKeepFiles // Files or directories to keep during a clean. Must be direct children of target
    //cleanKeepGlobs // Globs to keep during a clean. Must be direct children of target

    //val taskKeys = List(
    //  unmanagedSources,   // Unmanaged sources, which are manually created
    //  managedSources,     // Sources generated by the build
    //  sources,            // All sources, both managed and unmanaged
    //  unmanagedResources, // Unmanaged resources, which are manually created
    //  managedResources,   // Resources generated by the build
    //  resources,          // All resource files, both managed and unmanaged
    //  cleanFiles,         // The files to recursively delete during a clean
    //)

    val shisaMain = LocalProject("shisaMain")
    val shisaRoot = LocalProject("shisaRoot")

    val configs = List(Zero, Select[ConfigKey](Compile), Select[ConfigKey](Test))

    val allScopes = GlobalScope +: (for {
      ref  <- List(ThisBuild, shisaRoot, shisaMain)
      conf <- configs
    } yield Global.in(ref).copy(config = conf))

    val projScopes = (for {
      ref  <- List(shisaRoot, shisaMain)
      conf <- configs
    } yield Global.in(ref).copy(config = conf))

    val x = Project.extract(s)

    def showRef(r: Reference) = r match {
      case ThisBuild             => "ThisBuild"
      case LocalProject(project) => project
      case _                     => s"$r"
    }

    import scala.util.matching.Regex.quote
    def showS(scopes: Seq[Scope])(k: SettingKey[_]) = for (scope <- scopes) {
      val value_s = x.getOpt(scope / k).getOrElse("-").toString.replaceAll(quote("/d/shisa/"), "/").replaceAll(quote("/d/shisa"), "/")
      val ref_s   = scope.project.foldStrict(showRef, "", "This")
      val conf_s  = scope.config.foldStrict(_.name.capitalize, "", "This")
      println(f"$ref_s%-10s $conf_s%-10s ${k.key.label}%-30s $value_s")
    }

    boolKeys.foreach(showS(allScopes))
    showS(allScopes)(baseDirectory)
    dirKeys.foreach(showS(projScopes))
    dirsKeys.foreach(showS(projScopes))
    showS(projScopes)(cleanKeepFiles)
    showS(projScopes)(cleanKeepGlobs)

    s
  }
}
