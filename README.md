# shisa

A tool for comparing or testing the behaviour of nsc and dotc, and their various flags.

## Available

Implementations:
* Scala 2.13.1   (latest stable)
* Scala 2.13.x   (in dev)
* Dotty          (in dev)

Scala flags:

* -Xlint:eta-sam         The Java-defined target interface for eta-expansion was not annotated @FunctionalInterface.
* -Xlint:eta-zero        Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.
* -Xsource:<version>     Treat compiler input as Scala source for the specified version, see scala/bug#8126.

Dotty flags:

* -language:Scala2Compat
* -strict                Use strict type rules, which means some formerly legal code does not typecheck anymore.

Also consider:
* -Xmigration:<version>  Warn about constructs whose behavior may have changed since version.

Note: `<version>` is of form `major[.minor[.revision[-suffix]]]`, and missing mean 0.

## Combinations

| id       | version   | opts                   |
|----------|-----------|------------------------|
| base     | 2.13.1    |                        |
| 2.13     | 2.13.head |                        |
| 2.13-new | 2.13.head | -Xsource:3             |
| 3.0-old  | 3.0.head  | -language:Scala2Compat |
| 3.0      | 3.0.head  |                        |
| 3.1      | 3.0.head  | -strict                |
