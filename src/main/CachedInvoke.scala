package shisa

final class CachedInvoke(invoke: Invoke) extends Invoke {
  lazy val instance = invoke.mkRunner()

  def id         = invoke.id
  def cmd        = invoke.cmd
  def mkRunner() = instance
}
