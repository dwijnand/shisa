package shisa
package neat

import Shareds.*, Shareables.*
import Typeables.*

/** This module provides the 'Enumerable' class, which has a simple purpose: provide any enumeration for any instance type.
 * The prerequisite is that the enumeration data type is a Sized functor with the enumerated type as the type parameter.
 * The general idea is that the size of a value is the number of constructor applications it contains.
 *
 * Because Sized functors often rely of memoization, sharing is important.
 * Since class dictionaries are not always shared, a mechanism is added that guarantees optimal sharing
 * (it never creates two separate instance members for the same type).
 * This is why the type of 'enumerate' is `Shared[F, A]` instead of simply `F[A]`.
 * The technicalities of this memoization are not important,
 * but it means there are two modes for accessing an enumeration: 'local' and 'global'.
 * The former means sharing is guaranteed within this value, but subsequent calls to local may recreate dictionaries.
 * The latter guarantees optimal sharing even between calls.
 * It also means the enumeration will never be garbage collected,
 * so use with care in programs that run for extended periods of time and contains many (especially non-regular) types.
 *
 * Once a type has an instance, it can be enumerated in several ways (by instantiating 'global' to different types).
 * For instance `global: Enumerable.Count[Option[Boolean]]` would only count the number of lists of Option[Boolean] of each size.
 * `global: Values[Option[Boolean]]` would give the actual values for all sizes as lists.
 *
 * Instances can be constructed in three ways:
 *
 * 1: Manually by passing 'datatype' a list where each element is an application of the constructor functions 'c0', 'c1' etc,
 * so a data type like Maybe would have `enumerate = datatype [c0 Nothing, c1 Just]`.
 * This assumes all field types of all constructors are enumerable (recursive constructors work fine).
 * The functions passed to `cX` do not have to be constructors, but should be injective functions
 * (if they are not injective the enumeration will contain duplicates).
 * So "smart constructors" can be used, for instance the `Rational` datatype is defined by an injection from the natural numbers.
 *
 * 2: Automatically with Template Haskell ('deriveEnumerable').
 * A top level declaration like `@deriveEnumerable ''Maybe` would derive an instance for the `Maybe` data type.
 *
 * 3: Manually using the operations of a Sized functor to build a `Shareable f a` value, then apply 'share' to it.
 * To use other instances of 'Enumerable' use 'access'. */
trait Enumerable[A: Typeable]:
  def enumerate[F[_]: Sized: TypeableK]: Shared[F, A]

given [F[_]: Sized]: Sized[[A] =>> Shareable[F, A]] with
  extension [A](fa: Shareable[F, A])
    def map[B](f: A => B): Shareable[F, B]           = Shareable(fa.run(_).map(f))
    def <|>(f2: => Shareable[F, A]): Shareable[F, A] = Shareable(r => fa.run(r) <|> f2.run(r))
  extension [A, B](ff: Shareable[F, A => B])
    def <*> (fa: Shareable[F, A]): Shareable[F, B] = Shareable(r => ff.run(r) <*> fa.run(r))
  def pure[A](a: A): Shareable[F, A] = Shareable(_ => Sized[F].pure(a))
  def empty[A]: Shareable[F, A]      = Shareable(_ => Sized[F].empty[A])
  extension [A] (fa: Shareable[F, A])
    def pay: Shareable[F, A]                                           = Shareable(r => fa.run(r).pay)
    override def product[B](fb: Shareable[F, B]): Shareable[F, (A, B)] = Shareable(r => fa.run(r).product(fb.run(r)))
  override def fin(n: Int): Shareable[F, Int]                          = Shareable(_ => Sized[F].fin(n))
  override def finSized(i: Int): Shareable[F, Int]                     = Shareable(_ => Sized[F].finSized(i))
  override def naturals: Shareable[F, Int]                             = Shareable(_ => Sized[F].naturals)

object Enumerable:
  def apply[A](using z: Enumerable[A]) = z

  given Enumerable[Unit] with
    def enumerate[F[_]: Sized: TypeableK]: Shared[F, Unit] = Applicative[[A] =>> Shareable[F, A]].pure(()).share

  given [A: Enumerable, B: Enumerable]: Enumerable[(A, B)] with
    def enumerate[F[_]: Sized: TypeableK]: Shared[F, (A, B)] = access[A, F].product(access[B, F]).share
  given [A: Enumerable, B: Enumerable, C: Enumerable]: Enumerable[(A, B, C)] with
    def enumerate[F[_]: Sized: TypeableK]: Shared[F, (A, B, C)] = c1[F, (A, (B, C)), (A, B, C)] { case (a, (b, c)) => (a, b, c) }.share

  def access[A: Enumerable, F[_]: Sized: TypeableK]: Shareable[F, A] = Enumerable[A].enumerate[F].unsafeAccess

  val gref: Ref = Ref.unsafeNewRef()

  def local [F[_]: TypeableK: Sized, A: Enumerable]: F[A] = access[A, F].run(Ref.unsafeNewRef())
  def global[F[_]: TypeableK: Sized, A: Enumerable]: F[A] = access[A, F].run(gref)

  def datatype[A: Typeable, F[_]: Sized: TypeableK](ctors: List[Shareable[F, A]]): Shared[F, A] =
    S.aconcat(ctors).pay.share

  def c0[F[_]: Sized, A](a: A): Shareable[F, A] = S.pure(a)

  def c1[F[_]: Sized: TypeableK, A: Enumerable, X](f: A => X): Shareable[F, X] = access[A, F].map(f)

  def c2[F[_]: Sized: TypeableK, A: Enumerable, B: Enumerable, X](f: (A, B) => X): Shareable[F, X] =
    c1[F, (A, B), X] { case (a, b) => f(a, b) }

  def c3[F[_]: Sized: TypeableK, A: Enumerable, B: Enumerable, C: Enumerable, X](f: (A, B, C) => X): Shareable[F, X] =
    c2[F, A, (B, C), X] { case (a, (b, c)) => f(a, b, c) }

  given Enumerable[Boolean] with
    def enumerate[F[_]: Sized: TypeableK]: Shared[F, Boolean] = datatype(List(S.pure(false), S.pure(true)))

  given [A: Enumerable, B: Enumerable]: Enumerable[Either[A, B]] with
    def enumerate[F[_]: Sized: TypeableK]: Shared[F, Either[A, B]] = datatype(List(c1(Left(_: A): Either[A, B]), c1(Right(_: B): Either[A, B])))

  given [A: Enumerable]: Enumerable[List[A]] with
    def enumerate[F[_]: Sized: TypeableK]: Shared[F, List[A]] = datatype(List(S.pure(List.empty[A]), c2((a: A, as: List[A]) => a :: as)))

  given [A: Enumerable]: Enumerable[Option[A]] with
    def enumerate[F[_]: Sized: TypeableK]: Shared[F, Option[A]] = datatype(List(S.pure(None: Option[A]), c1((a: A) => Some(a))))

  private inline def S[F[_]: Sized]: Sized[[A] =>> Shareable[F, A]] = Sized[[A] =>> Shareable[F, A]]
end Enumerable