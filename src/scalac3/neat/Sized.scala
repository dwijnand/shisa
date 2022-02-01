package shisa
package neat

/** A sized functor is an applicative functor extended with a notion of cost/size of contained values.
 * This is useful for any type of bounded recursion over infinite sets, most notably for various kind of enumerations.
 *
 * The intention is that every sized functor definition models a (usually) infinite set (technically a bag)
 * with a finite number of values of any given size.
 * As long as every cyclic (recursive) definition has at least one application of pay, this invariant is guaranteed.
 *
 * The module "Control.Enumerable" provides sized functor definitions for a lot of data types,
 * such that the size of a value is the number of constructor applications it contains.
 * It also allows deriving these functors for any user defined data type. */
trait Sized[F[_]] extends Alternative[F]:
  extension [A] (fa: F[A])
    def pay: F[A]
  def aconcat[A](xs: List[F[A]]): F[A] = xs.foldLeft(empty[A])(_ <|> _)
  def fin(n: Int): F[Int]              = aconcat(List.tabulate(n)(pure))
  def finSized(i: Int): F[Int]         = Sized.finBits[F](i)(using this)
  def naturals: F[Int]                 = Sized.naturals[F](using this)
end Sized

object Sized:
  def apply[F[_]](using z: Sized[F]) = z

  def naturals[F[_]](using F: Sized[F]): F[Int] =
    def go(n: Int): F[Int] = (F.fin(2 ^^ n).map(2 ^^ n + _) <|> go(n + 1)).pay
    F.pure(0) <|> go(1)

  def naturals2[F[_]](using F: Sized[F]): F[Int] =
    def go(n: Int): F[Int] = (F.fin(n).map(n + _) <|> go(2 * n)).pay
    F.pure(0) <|> go(1)

  def finBits[F[_]](i: Int)(using F: Sized[F]): F[Int] =
    def go(n: Int): F[Int] =
      if      n <= lim then (F.fin(n).map(n + _) <|> go(2 * n)).pay
      else if n >= i   then F.empty
      else F.fin(i - n).map(n + _).pay
    def lim = i / 2
    if i <= 0 then F.empty else F.pure(0) <|> go(1)

  def kbits[F[_]](k: Int)(using F: Sized[F]): F[Int] = F.finSized(2 ^^ k)
end Sized