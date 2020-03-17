# shisa

A tool for comparing or testing the behaviour of Scala 2 and Scala 3, and its various flags.

Implementations:
* Scala 2.13     (latest stable)
* Scala 2.13.x   (in dev)
* Scala 3.0.next (latest prerelease)
* Scala 3.0.x    (in dev)

Scala 2 flags:

* -Xlint:eta-sam         The Java-defined target interface for eta-expansion was not annotated @FunctionalInterface.
* -Xlint:eta-zero        Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.
* -Xsource:<version>     Treat compiler input as Scala source for the specified version, see scala/bug#8126.

Scala 3 flags:

* -language:Scala2Compat
* -strict                Use strict type rules, which means some formerly legal code does not typecheck anymore.

Also consider:
* -Xmigration:<version>  Warn about constructs whose behavior may have changed since version.
* -migration             Emit warning and location for migration issues from Scala 2.

Note: `<version>` is of form `major[.minor[.revision[-suffix]]])`.
