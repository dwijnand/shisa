class Test {
  val any: Any    = ""
  val ref: AnyRef = ""

  any.##
  any.##()

  ref.##
  ref.##()
}
