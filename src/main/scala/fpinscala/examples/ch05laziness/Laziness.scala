class Laziness {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }
  // if2(false, sys.error("fail"), 3)
  // res2: Int = 3

  def maybeTwice(b: Boolean, i: => Int): Int = {
    if (b) i + i else 0
  }
  // val x = maybeTwice(true, { println("hi"); 1+41 })
  // hi     // <- one hi
  // hi     // <- second hi
  // x: Int = 84

  def maybeTwice2(b: Boolean, i: => Int): Int = {
    lazy val j = i
    if (b) j + j else 0
  }
  // val x = maybeTwice2(true, { println("hi"); 1+41 })
  // hi     // <- one hi only
  // x: Int = 84
}
