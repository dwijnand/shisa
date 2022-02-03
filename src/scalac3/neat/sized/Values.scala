package shisa
package neat

import MaxSizes.*

object Values:
  opaque type Values[A] = Int => List[A]

  given Sized[List] with
    extension [A](fa: List[A])
      def map[B](f: A => B): List[B]   = ???
      def <|>(f2: => List[A]): List[A] = ???
    extension [A, B](ff: List[A => B])
      def <*> (fa: List[A]): List[B] = ???
    def pure[A](a: A): List[A] = ???
    def empty[A]: List[A]      = ???
    extension [A] (fa: List[A])
      def pay: List[A]         = ???

  type FromInt[A] = Int => List[A]
  given Sized[FromInt] with
    extension [A](fa: Int => List[A])
      def map[B](f: A => B): Int => List[B]          = ???
      def <|>(f2: => Int => List[A]): Int => List[A] = ???
    extension [A, B](ff: Int => List[A => B])
      def <*> (fa: Int => List[A]): Int => List[B] = ???
    def pure[A](a: A): Int => List[A] = ???
    def empty[A]: Int => List[A]      = ???
    extension [A] (fa: Int => List[A])
      def pay: Int => List[A]         = ???

  object Values:
    extension [A] (self: Values[A])
      def run(n: Int): List[A] = self(n)

    def   ofSize[A: Enumerable](i: Int): List[A]       = Enumerable.global[FromInt, A].apply(i)
    def upToSize[A: Enumerable](i: Int): List[List[A]] = List.tabulate(i)(Enumerable.global[FromInt, A].apply(_))
    def      all[A: Enumerable]: List[List[A]]         =
      def aux(f: Values[A], m: MaxSize[A]): List[List[A]] = m.indices.map(f.run)
      aux(Enumerable.global[Values, A], Enumerable.global[MaxSize, A])

    given Sized[Values] with
      extension [A](fa: Values[A])
        def map[B](f: A => B): Values[B]   = ???
        def <|>(f2: => Values[A]): Values[A] = ???
      extension [A, B](ff: Values[A => B])
        def <*> (fa: Values[A]): Values[B] = ???
      def pure[A](a: A): Values[A] = ???
      def empty[A]: Values[A]      = ???
      extension [A] (fa: Values[A])
        def pay: Values[A]         = ???

object MaxSizes:
  opaque type MaxSize[A] = List[Unit]

  object MaxSize:
    extension [A] (self: MaxSize[A])
      def run: List[Unit]    = self
      def indices: List[Int] = self.indices.toList

    given Sized[MaxSize] with
      extension [A](fa: MaxSize[A])
        def map[B](f: A => B): MaxSize[B]   = ???
        def <|>(f2: => MaxSize[A]): MaxSize[A] = ???
      extension [A, B](ff: MaxSize[A => B])
        def <*> (fa: MaxSize[A]): MaxSize[B] = ???
      def pure[A](a: A): MaxSize[A] = ???
      def empty[A]: MaxSize[A]      = ???
      extension [A] (fa: MaxSize[A])
        def pay: MaxSize[A]         = ???
