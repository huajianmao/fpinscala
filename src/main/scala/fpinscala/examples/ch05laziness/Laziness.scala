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

  // Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  // Cons(11, Stream(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
  // Stream(2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  // Cons(12, Stream(3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
  // Cons(12, Stream(3, 4).map(_ + 10).filter(_ % 2 == 0)).toList
  // 12::Stream(3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  // 12::Cons(13, Stream(4).map(_ + 10)).filter(_ % 2 == 0).toList
  // 12::Stream(4).map(_ + 10).filter(_ % 2 == 0).toList
  // 12::Cons(14, Empty.map(_ + 10)).filter(_ % 2 == 0).toList
  // 12::Cons(14, Empty.map(_ + 10).filter(_ % 2 == 0)).toList
  // 12:;14::Empty.map(_ + 10).filter(_ % 2 == 0).toList
  // 12::14::Empty.filter(_ % 2 == 0).toList
  // 12::14::Empty.toList
  // 12::14::List()
  // 12::14

  // val ones: Stream[Int] = Stream.cons(1, ones)
}
