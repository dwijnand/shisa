package shisa

import scala.meta._, contrib._

object autoApp {
  def apply(encl: Defn, meth: Term.Name) = {
    val msg2 = s"""Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method $meth,
                  |or remove the empty argument list from its definition (Java-defined methods are exempt).
                  |In Scala 3, an unapplied method like this will be eta-expanded into a function.""".stripMargin
    Msgs(
      List(Msg(W, msg2)),
      List(Msg(W, msg2)),
      List(Msg(W, s"method $meth must be called with () argument")),
      List(Msg(E, s"method $meth in $encl must be called with () argument")),
      List(Msg(E, s"method $meth in $encl must be called with () argument")),
      List(Msg(E, s"method $meth in $encl must be called with () argument")),
    )
  }
}
