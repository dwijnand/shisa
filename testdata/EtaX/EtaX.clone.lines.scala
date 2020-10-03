class Test {
  val ys = { val t = scala.collection.mutable.Map(1 -> "foo"); t.clone } // ?/?/ok
}
