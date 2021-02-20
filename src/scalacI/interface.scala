package shisa

final case class SrcFile(name: String, content: String)

sealed trait Sev
case object W extends Sev
case object E extends Sev

final case class Msg(sev: Sev, text: String)

object Msg {
  val NoMsg = Msg(E, "<no message>")
}

trait Compiler {
  def compile1(src: SrcFile): List[Msg]
}

trait MkCompiler {
  def id: String
  def mkCompiler: Compiler
}
