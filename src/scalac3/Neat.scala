package shisa
package neat

import DynMaps.*
import IOs.*
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
    Sized[[A] =>> Shareable[F, A]].aconcat(ctors).pay.share

  def c0[F[_]: Sized, A](a: A): Shareable[F, A] = Applicative[[A] =>> Shareable[F, A]].pure(a)

  def c1[F[_]: Sized: TypeableK, A: Enumerable, X](f: A => X): Shareable[F, X] = access[A, F].map(f)

  def c2[F[_]: Sized: TypeableK, A: Enumerable, B: Enumerable, X](f: (A, B) => X): Shareable[F, X] =
    c1[F, (A, B), X] { case (a, b) => f(a, b) }

  def c3[F[_]: Sized: TypeableK, A: Enumerable, B: Enumerable, C: Enumerable, X](f: (A, B, C) => X): Shareable[F, X] =
    c2[F, A, (B, C), X] { case (a, (b, c)) => f(a, b, c) }
end Enumerable

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

type Ref = IORef[DynMap]
object Ref:
  def unsafeNewRef(): Ref = newRef.unsafeRun
  def newRef: IO[Ref]     = IORef.newIORef(DynMap.empty)

object Shareds:
  opaque type Shared[F[_], A] = Shareable[F, A]

  object Shared:
    def apply[F[_], A](x: Shareable[F, A]): Shared[F, A] = x

  extension [F[_], A] (x: Shared[F, A])
    /** Should only be used to access class members.
     *  A safe wrapper should be defined for every shared class member.
     *  Direct access can lead to overriding class member definitions. */
    def unsafeAccess: Shareable[F, A] = x
end Shareds

object Shareables:
  opaque type Shareable[F[_], A] = Ref => F[A]

  object Shareable:
    def apply[F[_], A](x: Ref => F[A]): Shareable[F, A] = x

  extension [F[_], A] (x: Shareable[F, A])
    def run(ref: Ref): F[A] = x(ref)

    /** Share/memoize a class member of type `F[A]` */
    def share(using FA: Typeable[F[A]]): Shared[F, A] =
      def memo[X: Typeable](x: X, r: Ref) = x.protect(r).unsafeRun
      Shared(r => memo(x.run(r), r))

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
end Shareables

extension [A: Typeable] (a: A)
  def protect(ref: Ref): IO[A] =
    for
      m <- ref.readIORef
      r <- m.lookup[A] match
        case Some(y) => IO.pure(y)
        case None    => ref.writeIORef(m.insert(a)) *> IO.pure(a)
    yield r
end extension

object DynMaps:
  opaque type DynMap = Map[TypeRep, Dynamic]

  object DynMap:
    def empty: DynMap = Map.empty

  extension (m: DynMap)
    def insert[A: Typeable](a: A): DynMap = m.updated(a.typeOf, a.toDyn)
    def lookup[A: Typeable]: Option[A]    = m.get(TypeRep.of[A]).flatMap(Typeable.fromDynamic)
end DynMaps

final case class IORef[A](var value: A)

object IORef:
  def newIORef[A](x: A): IO[IORef[A]] = IO.pure(IORef[A](x))

extension [A] (x: IORef[A])
  def readIORef: IO[A]        = IO(x.value)
  def writeIORef(a: A): IO[A] = IO { x.value = a; a }

object IOs:
  opaque type IO[A] = () => A

  object IO:
    def apply[A](a: => A): IO[A] = () => a
    def pure[A](a: A): IO[A]     = () => a

    given Monad[IO] with
      def pure[A](a: A): IO[A] = IO.pure(a)
      extension [A](fa: IO[A])
        def flatMap[B](f: A => IO[B]): IO[B] = f(fa())

  extension [A] (x: IO[A])
    def unsafeRun: A = x()
end IOs

object Typeables:
  opaque type TypeRep         = Class[_]
  opaque type Dynamic         = (TypeRep, Any)
  opaque type Typeable[A]     = scala.reflect.ClassTag[A]
         type TypeableK[A[_]] = Typeable[A[Any]]

  object TypeRep:
    def of[A: Typeable]: TypeRep = (Typeable[A]: scala.reflect.ClassTag[A]).runtimeClass.asInstanceOf

  extension [A: Typeable] (x: A)
    def typeOf: TypeRep = TypeRep.of[A]
    def toDyn: Dynamic  = x.asInstanceOf[Dynamic]

  object Typeable:
    def apply[A](using z: Typeable[A]) = z
    def fromDynamic[A: Typeable](d: Dynamic): Option[A] = d match
      case (t, v) if t == TypeRep.of[A] => Some(v.asInstanceOf[A])
      case _                            => None
    given [T](using ctag: scala.reflect.ClassTag[T]): Typeable[T] = ctag
end Typeables