package shisa
package neat

object Counts:
  /** Counts the number of values of all sizes. Usage: `global: Count[Boolean]` */
  opaque type Count[A] = List[Int]

  object Count:
    extension [A] (self: Count[A])
      def count: List[Int] = self

      /** Switch phantom type */
      def untyped[B]: Count[B] = self

      def compact: Count[A] = self.reverse.dropWhile(_ == 0).reverse

      def indexStar(n: Int): Int = self match
        case Nil               => 0
        case x :: xs if n <  0 => 0
        case x :: xs if n == 0 => x
        case x :: xs           => (xs: Count[A]).indexStar(n - 1)

    given Sized[Count] with
      extension [A](fa: Count[A])
        def map[B](f: A => B): Count[B]    = fa
        def <|>(f2: => Count[A]): Count[A] =
          def zipWithL(xs: List[Int], ys: List[Int]): Count[A] = (xs, ys) match
            case (x :: xs, y :: ys) => (x + y) :: zipWithL(xs, ys)
            case (Nil, ys)          => ys
            case (xs, Nil)          => xs
          zipWithL(fa, f2)
      extension [A, B](ff: Count[A => B])
        def <*> (fa: Count[A]): Count[B] = (ff, fa) match
          case (Nil, _)              => empty
          case (_, Nil)              => empty
          case (0 :: xs, ys)         => ((xs: Count[A => B]) <*> ys).pay
          case (xs, 0 :: ys)         => (xs <*> (ys: Count[A])).pay
          case (xs0 @ _ :: xs0P, ys) =>
            def mult(r: List[Int]) = conv(xs0, r)
            def run(xs: List[List[Int]]) = xs match
              case Nil     => Nil
              case r :: rs => go(r, rs)
            def go(r: List[Int], rs: List[List[Int]]): List[Int] =
              mult(r) :: rs.match
                case Nil       => goP(r, xs0P)
                case rP :: rsP => go(rP, rsP)
            def goP(r: List[Int], xs: List[Int]): List[Int] = xs match
              case Nil      => Nil
              case _ :: xsP => conv(r, xs) :: goP(r, xsP)
            run(reversalsP(ys).drop(1))
      def pure[A](a: A): Count[A] = List(1)
      def empty[A]: Count[A]      = List()
      extension [A] (fa: Count[A])
        def pay: Count[A] = 0 :: fa
      override def fin(n: Int): Count[Int] = List(n)
      override def aconcat[A](xs: List[Count[A]]): Count[A] = xs match
        case Nil        => empty
        case List(x)    => x
        case List(x, y) => x <|> y
        case xss        => xss.map(x => x: List[Int]).transpose.map(_.sum)

    def reversalsP[A](xs: List[A]): List[List[A]] =
      def go(rs: List[A], xs: List[A]): List[List[A]] = xs match
        case Nil     => Nil
        case x :: xs => go(x :: rs, xs)
      go(Nil, xs)

    def conv(xs: List[Int], ys: List[Int]): Int = xs.lazyZip(ys).map(_ * _).sum