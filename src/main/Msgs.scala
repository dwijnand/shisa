package shisa

import scala.meta._, contrib._

object Msgs {
  val None = Msgs(Nil, Nil, Nil, Nil, Nil, Nil)
  def apply(): Msgs = None

  def for2(f: Sev => Msg) = Msgs(List(f(W)), List(f(E)),        Nil,        Nil,        Nil,        Nil)
  def for3(f: Sev => Msg) = Msgs(       Nil,        Nil, List(f(W)), List(f(E)), List(f(W)), List(f(E)))
}

final case class Msgs(
    m2rg: List[Msg], m2nw: List[Msg],
    m30m: List[Msg], m30r: List[Msg], m31m: List[Msg], m31r: List[Msg],
) {
  def :::(msgs: Msgs) = Msgs(m2rg ::: msgs.m2rg, m2nw ::: msgs.m2nw,
    m30m ::: msgs.m30m, m30r ::: msgs.m30r, m31m ::: msgs.m31m, m31r ::: msgs.m31r)

  def ++(msgs: Msgs) = Msgs(add(m2rg, msgs.m2rg), add(m2nw, msgs.m2nw),
    add(m30m, msgs.m30m), add(m30r, msgs.m30r), add(m31m, msgs.m31m), add(m31r, msgs.m31r))

  private def add(a: List[Msg], b: List[Msg]) = if (a.exists(_.sev == E)) a else a ::: b

  def for2: Msgs = Msgs(m2rg, m2nw, Nil, Nil, Nil, Nil)
  def for3: Msgs = Msgs(Nil, Nil, m30m, m30r, m31m, m31r)

  def toList  = List(m2rg) // , m2nw, m30m, m30r, m31m, m31r)
  def isEmpty = this == Msgs()
}
