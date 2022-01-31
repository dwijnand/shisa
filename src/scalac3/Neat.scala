package shisa
package neat

trait Sized[F[_]] extends Alternative[F]:
  extension [A] (fa: F[A])
    def pay: F[A]
    def pair[B](fb: F[B]): F[(A, B)] = fa.product(fb)
  def aconcat[A](xs: List[F[A]]): F[A] = xs.foldLeft(empty[A])(_ <|> _)
  def fin(n: Int): F[Int]      = aconcat(List.tabulate(n)(pure))
  def finSized(i: Int): F[Int] = stdFinBits[F](i)(using this)
  def naturals: F[Int]         = stdNaturals[F](using this)

def stdNaturals[F[_]](using F: Sized[F]): F[Int] =
  def go(n: Int): F[Int] = (F.fin(2 ^^ n).map(2 ^^ n + _) <|> go(n + 1)).pay
  F.pure(0) <|> go(1)

def stdNaturals2[F[_]](using F: Sized[F]): F[Int] =
  def go(n: Int): F[Int] = (F.fin(n).map(n + _) <|> go(2 * n)).pay
  F.pure(0) <|> go(1)

def stdFinBits[F[_]](i: Int)(using F: Sized[F]): F[Int] =
  def go(n: Int): F[Int] =
    if      n <= lim then (F.fin(n).map(n + _) <|> go(2 * n)).pay
    else if n >= i   then F.empty
    else F.fin(i - n).map(n + _).pay
  def lim = i / 2
  if i <= 0 then F.empty else F.pure(0) <|> go(1)

def kbits[F[_]](k: Int)(using F: Sized[F]): F[Int] = F.finSized(2 ^^ k)

extension (i: Int) private def ^^ (j: Int) = math.pow(i, j).toInt
