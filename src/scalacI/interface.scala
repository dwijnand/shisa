package shisa

final case class SrcFile(name: String, content: String)

sealed trait Sev
case object W extends Sev
case object E extends Sev

trait Msg {
  def sev: Sev
  def text: String

  final override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: Msg => this.sev == that.sev && this.text == that.text
    case _         => false
  })
}

final case class MsgImpl(sev: Sev, text: String) extends Msg {
  override def productPrefix: String = "Msg"
}

object Msg {
  def apply(sev: Sev, text: String): Msg     = MsgImpl(sev, text)
  def unapply(msg: Msg): Some[(Sev, String)] = Some((msg.sev, msg.text))
  val NoMsg = Msg(E, "<no message>")
}

trait Compiler {
  def compile1(src: SrcFile): List[Msg]
}

trait MkCompiler {
  def id: String
  def mkCompiler: Compiler
}
