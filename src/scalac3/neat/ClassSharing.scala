package shisa
package neat

import DynMaps.*
import IOs.*
import Shareds.*, Shareables.*
import Typeables.*

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
  /** A dynamic map with type safe insertion and lookup. */
  opaque type DynMap = Map[TypeRep, Dynamic]

  object DynMap:
    def empty: DynMap = Map.empty

  extension (m: DynMap)
    def insert[A: Typeable](a: A): DynMap = m.updated(a.typeOf, a.toDyn)
    def lookup[A: Typeable]: Option[A]    = m.get(TypeRep.of[A]).flatMap(Typeable.fromDynamic)
end DynMaps