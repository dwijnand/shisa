trait A { def boom(): Unit }

class Test {
  new A().boom // ?/?/err: apply, ()-insertion
}
