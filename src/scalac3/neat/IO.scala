package shisa
package neat

import IOs.*

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