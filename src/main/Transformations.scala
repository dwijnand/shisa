package shisa

import scala.meta._, classifiers.Classifier, contrib._
import nme._, tpnme._

object Transformations {
  implicit class ListOps[T](private val xs: List[T]) extends AnyVal {
    def has[U](implicit classifier: Classifier[T, U]): Boolean = xs.exists(classifier(_))

    def  appendOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else xs :+ x
    def prependOnce[U](x: T)(implicit classifier: Classifier[T, U]) = if (xs.has[U]) xs else x :: xs
  }

  implicit class ExtensionOps[A](private val a: A) extends AnyVal {
    def append [B](b: B )(implicit M: Modify[A, B]                         ): A = M.modify(a, _ :+ b)
    def dropMod[M <: Mod](implicit M: Modify[A, Mod], C: Classifier[Mod, M]): A = M.modify(a, _.filter(_.isNot[M]))
    def withMod  (m: Mod)(implicit M: Modify[A, Mod]                       ): A = M.modify(a, _.appendOnce(m))
  }

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def addInit(init: Init) = cls.copy(templ = cls.templ.copy(inits = cls.templ.inits.prependOnce(init)))

    def toCaseClass = cls.copy(
      mods = cls.mods.appendOnce(Mod.Case()),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil     => List(Nil)
        case paramss => paramss.map(_.map(_.dropMod[Mod.ValParam]))
      }),
    )

    def toValueClass(param: Term.Param) = cls.addInit(inits.AnyVal).copy(
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case (p :: _) :: _ => List(List(p.withMod(Mod.ValParam())))
        case _             => List(List(param))
      }),
    )

    def isCaseClass  = cls.hasMod(Mod.Case())
    def isValueClass = cls.templ.inits.exists { case Init(t"AnyVal", Name(""), Nil) => true case _ => false }
  }
}

trait Modify[A, B] {
  def modify(a: A, f: List[B] => List[B]): A
}

object Modify {
  implicit def derive[A, B](implicit E: Extract[A, B], R: Replace[A, B]): Modify[A, B] =
    (a, f) => R.replace(a, f(E.extract(a)))
}

object nme {
  val getClass_ = q"getClass"
  val hashCode_ = q"hashCode"
  val hashHash  = q"##"
  val run       = q"run"
  val toString_ = q"toString"
}

object tpnme {
  val Any    = t"Any"
  val AnyRef = t"AnyRef"
  val AnyVal = t"AnyVal"
  val Int    = t"Int"
  val Object = t"Object"
  val String = t"String"
}

object inits {
  val AnyVal = init"AnyVal"
}
