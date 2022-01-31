package shisa
package feat

type N = Int

case class Finite2[A](card: N, apply: N => A)

object Finite:
  def empty[A]: Finite[A]           = ???
  def singleton[A](a: A): Finite[A] = ???

  def singleton2[A](a: A): Finite2[A] =
    val one: N => A =
      case 0 => a
      case _ => sys.error("Index out of bounds")
    Finite2(1, one)

  def empty2[A]: Finite2[A]         = Finite2(0, _ => ???)

  def concat[A](xs: List[Finite[A]]) = xs.foldLeft(empty[A])(_ + _)

abstract class Finite[A]:
  def card: N
  def at(i: N): A
  def values: List[A] = List.tabulate(card)(at)

  def +(b: Finite[A]): Finite[A] =
    val car = card + b.card
    def ix(i: N) = if i < card then at(i) else b.at(i - card)
    new Finite[A]:
      override def card     = car
      override def at(i: N) = ix(i)

  def <+>[B](b: Finite[B]): Finite[Either[A, B]] = ???

  def <*>[B](b: Finite[B]): Finite[(A, B)] =
    val car       = card * b.card
    def sel(i: N) =
      import scala.math.Integral.Implicits._
      val (q, r) = (i: Int) /% b.card
      (at(q), b.at(r))
    new Finite[(A, B)]:
      override def card     = car
      override def at(i: N) = sel(i)

  def biMap[B](f: A => B): Finite[B]             = ???

type Part = N
type E[A] = Part => Finite[A]

object E:
  def empty[A]: E[A]           = Function.const(Finite.empty[A])
  def singleton[A](a: A): E[A] = { case 0 => Finite.singleton(a) case _ => Finite.empty }

extension [A] (e: E[A])
  def index(i: N): A =
    def go(p: Int, i: N): A = if i < e(p).card then e(p).at(i) else go(p + 1, i - e(p).card)
    go(0, i)

  def biMap[B](f: A => B): E[B] = ((_: Finite[A]).biMap(f)).compose(e)

  // disjoint union
  def <+>[B](e2: E[B]): E[Either[A, B]] = p => e(p) <+> e2(p)

  def union(e2: E[A]): E[A] = (e <+> e2).biMap(_.merge)

  def pay: E[A] = ???

  def cost(t: A): N = ???

  def <*>[B](e2: E[B]): E[(A, B)] = p => Finite.concat(List.tabulate(p)(k => e(k) <*> e2(k)))

case class Enumerate[A](parts: List[Finite[A]]):
  def index(i: N): A =
    def index1(xs: List[Finite[A]], i: N): A = xs match
      case Nil                     => sys.error("index out of bounds")
      case f :: rest if i < f.card => f.at(i)
      case f :: rest               => index1(rest, i - f.card)
    index1(parts, i)

  def <+>(b: Enumerate[A]): Enumerate[A] =
    def zipPlus[X](xs: List[X], ys: List[X])(f: (X, X) => X): List[X] = (xs, ys) match
      case (x :: xs, y :: ys) => f(x, y) :: zipPlus(xs, ys)(f)
      case _                  => xs ::: ys // one of them is empty
    Enumerate(zipPlus(parts, b.parts)(_ + _))

object Enumerate:
  def empty[A]: Enumerate[A]           = Enumerate[A](List())
  def singleton[A](a: A): Enumerate[A] = Enumerate[A](List(Finite.singleton(a)))

final case class Prop[A](lhs: A, rhs: A)

extension [A] (x: A) def ===(y: A) = Prop(x, y)

object E_Properties:
  def indexPay[A](e: E[A], i: N)                           = e.pay.index(i)                === e.index(i)
  def indexTwice[A](e: E[A], i1: N, i2: N)                 = (e.index(i1) === e.index(i2)) === (i1 === i2)
  def payPlus[A](e1: E[A], e2: E[A])                       = (e1 <+> e2).pay               === e1.pay <+> e2.pay
  def payTimes1[A](e1: E[A], e2: E[A])                     = (e1 <*> e2).pay               === e1.pay <*> e2
  def payTimes2[A](e1: E[A], e2: E[A])                     = (e1 <*> e2).pay               === e1 <*> e2.pay
//def fixPay[A](e: E[A])                                   = e.pay.fix === empty
  def biMapCompose[A, B, C](e: E[A], f: B => C, g: A => B) = e.biMap(g).biMap(f)           === e.biMap(f.compose(g))
  def singletonTimes[A, B](a: A, e: E[B])                  = E.singleton(a) <*> e          === e.biMap((a, _))
  def timesSingleton[A, B](e: E[A], b: B)                  = e <*> E.singleton(b)          === e.biMap((_, b))
  def emptyPlus[A, B](e: E[B])                             = E.empty[A] <+> e              === e.biMap(Right(_))
  def plusEmpty[A, B](e: E[A])                             = e <*> E.empty[B]              === e.biMap(Left(_))

  def costSingleton[A](t1: A, t2: A)                       = E.singleton(t1).cost(t2) === 0
  def costPlusL[A, B](a: E[A], b: E[B], x: A)              = (a <+> b).cost( Left(x)) === a.cost(x)
  def costPlusR[A, B](a: E[A], b: E[B], x: B)              = (a <+> b).cost(Right(x)) === b.cost(x)
  def costTimes[A, B](a: E[A], b: E[B], x: A, y: B)        = (a <*> b).cost((x, y))   === a.cost(x) + b.cost(y)
  def costPlusL[A, B](e: E[A], f: A => B, g: B => A, x: B) = e.biMap(f).cost(x)       === e.cost(g(x))
  def costPlusL[A   ](e: E[A], x: A)                       = e.pay.cost(x)            === 1 + e.cost(x)

object FiniteProperties:
  def cardValuesLength[A](f: Finite[A])  = f.card === f.values.length
  def indexValues[A](f: Finite[A], i: N) = f.at(i)   === f.values(i)

  def emptyValues[A]                                  = Finite.empty[A].values === List[A]()
  def singletonValues[A](a: A)                        = Finite.singleton(a).values === List(a)
  def  plusValues[A, B](f1: Finite[A], f2: Finite[B]) = (f1 <+> f2).values === f1.values.map(Left(_)) ++ f2.values.map(Right(_))
  def timesValues[A, B](f1: Finite[A], f2: Finite[B]) = (f1 <*> f2).values === (for (x <- f1.values; y <- f2.values) yield (x, y))
  def biMapValues[A, B](f: Finite[A], g: A => B)      = f.biMap(g).values === f.values.map(g)

object EnumExamples:
  enum N { case Z; case S(prev: N) }; import N.*
  def natEnum: E[N] = E.singleton(Z).union(natEnum.biMap(S(_))).pay