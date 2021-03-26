package shisa

import scala.meta._, classifiers.Classifier, contrib._, equality._
import nme._, tpnme._

object Transformations {
  implicit class ListOps[T](private val xs: List[T]) extends AnyVal {
    def has(x: T)(implicit Eq: Equal[T]): Boolean = xs.exists(a => (a.asInstanceOf[AnyRef] eq x.asInstanceOf[AnyRef]) || Eq.isEqual(a, x))

    def  appendOnce(x: T)(implicit Eq: Equal[T]) = if (xs.has(x)) xs else xs :+ x
    def prependOnce(x: T)(implicit Eq: Equal[T]) = if (xs.has(x)) xs else x :: xs
  }

  implicit class ExtensionOps[A](private val a: A) extends AnyVal {
    def append     [B](b: B)                 (implicit M: Modify[A, B  ]): A = M.modify(a, _ :+ b)
    def prependOnce[B: Equal](b: B)          (implicit M: Modify[A, B  ]): A = M.modify(a, _.prependOnce(b))
    def dropMod[M <: Mod: Classifier[Mod, *]](implicit M: Modify[A, Mod]): A = M.modify(a, _.filter(_.isNot[M]))
    def withMod  (m: Mod)                    (implicit M: Modify[A, Mod]): A = M.modify(a, _.appendOnce(m))
  }

  implicit class DefnClassOps(private val cls: Defn.Class) extends AnyVal {
    def isCaseClass  = cls.hasMod(Mod.Case())
    def isValueClass = cls.templ.inits.exists { case Init(t"AnyVal", Name(""), Nil) => true case _ => false }

    def addInit(init: Init) = cls.prependOnce(init)

    def toCaseClass = cls.copy(
      mods = cls.mods.appendOnce(Mod.Case()),
      ctor = cls.ctor.copy(paramss = cls.ctor.paramss match {
        case Nil     => List(Nil)
        case paramss => paramss.dropMod[Mod.ValParam]
      }),
    )

    def toValueClass(param: Term.Param) = cls.addInit(inits.AnyVal).copy(
      ctor = cls.ctor.copy(paramss = (cls.ctor.paramss match {
        case (param :: _) :: _ => List(List(param))
        case _                 => List(List(param))
      }).withMod(Mod.ValParam())),
    )
  }

  implicit val extractTemplateInits:  Extract[Template,   Init] = Extract(_.inits)
  implicit val replaceTemplateInits:  Replace[Template,   Init] = Replace((a, bs) => a.copy(inits = bs))
  implicit val extractClassDefnInits: Extract[Defn.Class, Init] = Extract[Template, Init].contramap(_.templ)
  implicit val replaceClassDefnInits: Replace[Defn.Class, Init] = Replace[Template, Init].imap(_.templ, (c, t) => c.copy(templ = t))

  implicit def extractXs[A]: Extract[List[A], A]                             = Extract(xs => xs)
  implicit def modifyXxs[A, B](implicit M: Modify[A, B]): Modify[List[A], B] = (as, f) => as.map(M.modify(_, f))

  implicit def eqTree[T <: Tree]: Equal[T] = Structurally.equal(_, _)

  def Extract[A, B](implicit E: Extract[A, B]) = E
  def Replace[A, B](implicit R: Replace[A, B]) = R

  implicit class ExtractOps[A, B](private val E: Extract[A, B]) extends AnyVal {
    def contramap[C](f: C => A): Extract[C, B] = Extract(c => E.extract(f(c)))
  }
  implicit class ReplaceOps[A, B](private val E: Replace[A, B]) extends AnyVal {
    def imap[C](f: C => A, g: (C, A) => C): Replace[C, B] = Replace((c, bs) => g(c, E.replace(f(c), bs)))
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
  val hashHash  = q"##"
  val toString_ = q"toString"
  val Null      = Lit.Null()
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
