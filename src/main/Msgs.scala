package shisa

object Variants {
  val None: Variants[Nothing] = Variants(Nil, Nil, Nil, Nil, Nil, Nil)
  def apply[A](): Variants[A] = None
}

final case class Variants[+A](
    a2rg: List[A], a2nw: List[A],
    a30m: List[A], a30r: List[A], a31m: List[A], a31r: List[A],
) {
  def :::[B >: A](rhs: Variants[B]) = Variants(a2rg ::: rhs.a2rg, a2nw ::: rhs.a2nw,
    a30m ::: rhs.a30m, a30r ::: rhs.a30r, a31m ::: rhs.a31m, a31r ::: rhs.a31r)

  def for2: Variants[A] = Variants(a2rg, a2nw, Nil, Nil, Nil, Nil)
  def for3: Variants[A] = Variants(Nil, Nil, a30m, a30r, a31m, a31r)

  def toList  = List(a2rg, a2nw, a30m, a30r, a31m, a31r)
  def isEmpty = this == Variants.None
}

object MsgsImport {
  type Msgs = Variants[Msg]

  implicit class MsgsOps(private val msgs: Msgs) extends AnyVal {
    import msgs._

    def ++(msgs: Msgs) = Msgs(add(a2rg, msgs.a2rg), add(a2nw, msgs.a2nw),
      add(a30m, msgs.a30m), add(a30r, msgs.a30r), add(a31m, msgs.a31m), add(a31r, msgs.a31r))

    private def add(a: List[Msg], b: List[Msg]) = if (a.exists(_.sev == E)) a else a ::: b
  }
}
import MsgsImport._

object Msgs {
  val None: Msgs    = Variants.None
  def apply(): Msgs = None
  def apply(
      m2rg: List[Msg], m2nw: List[Msg],
      m30m: List[Msg], m30r: List[Msg], m31m: List[Msg], m31r: List[Msg],
  ): Msgs = Variants(m2rg, m2nw, m30m, m30r, m31m, m31r)

  def for2(f: Sev => Msg)  = Msgs(List(f(W)), List(f(E)),        Nil,        Nil,        Nil,        Nil)
  def for3(f: Sev => Msg)  = Msgs(       Nil,        Nil, List(f(W)), List(f(E)), List(f(W)), List(f(E)))
  def for30(f: Sev => Msg) = Msgs(       Nil,        Nil,        Nil, List(f(E)), List(f(E)), List(f(E)))
  def for3F(f: Sev => Msg) = Msgs(       Nil,        Nil, List(f(W)), List(f(E)), List(f(E)), List(f(E)))

  implicit class MsgsOps(private val msgs: Msgs) extends AnyVal {
    import msgs._

    def ++(msgs: Msgs) = Msgs(add(a2rg, msgs.a2rg), add(a2nw, msgs.a2nw),
      add(a30m, msgs.a30m), add(a30r, msgs.a30r), add(a31m, msgs.a31m), add(a31r, msgs.a31r))

    private def add(a: List[Msg], b: List[Msg]) = if (a.exists(_.sev == E)) a else a ::: b
  }
}
