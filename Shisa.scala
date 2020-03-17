package shisa

object Main {
  lazy val combinations = Seq(
    Scala("base",     "-2.13.1"),
    Scala("2.13",     "-2.13.head"),
    Scala("2.13-new", "-2.13.head -Xsource:3"),
    Dotty("3.0-old",  "-language:Scala2Compat"),
    Dotty("3.0",      ""),
    Dotty("3.1",      "-strict"),
  )

  def main(args: Array[String]): Unit = {
  }

  def tokenise(s: String) = s.split(' ')
}

sealed trait Invoke
final case class Scala(id: String, opts: String) extends Invoke
final case class Dotty(id: String, opts: String) extends Invoke
