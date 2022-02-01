package shisa
package neat

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