package fpinscala.exercises.ch07parallelism

object Par {
  type Par[A] = Nothing // FIXME:

  // scalastyle:off noimpl
  def unit[A](a: => A): Par[A] = { ??? }
  def get[A](par: Par[A]): A = { ??? }

  /**
   * Exercise 7.1
   *
   * Par.map2 is a new higher-order function
   * for combining the result of two parallel computations.
   * What is its signature?
   * Give the most general signature possible
   * (don't assume it works only for Int).
   */
  def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = {
    ???
  }
  // scalastyle:on noimpl

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }
}
