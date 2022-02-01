package shisa
package neat

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]
    def as[B](b: B): F[B] = map(_ => b)
    def void: F[Unit] = as(())
  def lift[A, B](f: A => B): F[A] => F[B] = _.map(f)

trait Semigroupal[F[_]]:
  extension [A] (fa: F[A])
    def product[B](fb: F[B]): F[(A, B)]

trait Semigroup[A]:
  extension (x: A)
    def |+| (y: A): A

object Semigroup:
  given Semigroup[Int] with
    extension (x: Int)
      def |+| (y: Int) = x + y

trait Monoid[A] extends Semigroup[A]:
  def empty: A

trait Apply[F[_]] extends Functor[F], Semigroupal[F]:
  extension [A, B](ff: F[A => B])
    def <*> (fa: F[A]): F[B]

  extension [A](fa: F[A])
    def map2[B, Z](fb: F[B])(f: (A, B) => Z): F[Z] = fa.product(fb).map(f.tupled)
    def product[B](fb: F[B]): F[(A, B)]            = fa.map(a => (b: B) => (a, b)) <*> fb
    def <*[B](fb: F[B]): F[A]                      = fa.map2(fb)((a, _) => a)
    def *>[B](fb: F[B]): F[B]                      = fa.map2(fb)((_, b) => b)

trait Applicative[F[_]] extends Apply[F]:
  def pure[A](a: A): F[A]
  def unit: F[Unit] = pure(())

object Applicative:
  def apply[F[_]](using z: Applicative[F]) = z

trait Alternative[F[_]] extends Applicative[F]:
  extension [A] (f: F[A])
    def <|>(f2: => F[A]): F[A]
  def empty[A]: F[A]

object Alternative:
  def apply[F[_]](using z: Alternative[F]) = z

trait FlatMap[F[_]] extends Apply[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

  extension [A, B](ff: F[A => B])
    def <*>(fa: F[A]): F[B] = ff.flatMap(f => fa.map(f))

  extension [A](ffa: F[F[A]])
    def flatten: F[A] = ffa.flatMap(identity)

trait Monad[F[_]] extends FlatMap[F], Applicative[F]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B] = fa.flatMap(f.andThen(pure))

extension (i: Int) private def ^^ (j: Int) = math.pow(i, j).toInt