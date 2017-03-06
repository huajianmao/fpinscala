package fpinscala.exercises.ch05laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
   * Exercise 5.1
   *
   * Write a function to convert a Stream to a List,
   * which will force its evaluation and let you look at it in the REPL.
   * You can convert to the regular List type in the standard library.
   * You can place this and other functions that operate on a Stream
   * inside the Stream trait.
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  /**
   * Exercise 5.2
   *
   * Write the function take(n) for returning the first n elements of a Stream,
   * and drop(n) for skipping the first n elements of a Stream.
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    case _ => Empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  /**
   * Exercise 5.3
   * Write the function takeWhile for returning all starting elements of a Stream that
   * match the given predicate.
   * def takeWhile(p: A => Boolean): Stream[A]
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }


  /**
   * Code for Example in Section 5.3
   */
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) || t().exists(p) => true
    case _ => false
  }
  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def existsViaFoldRight(p: A => Boolean): Boolean = {
    foldRight(false)(p(_) || _)
  }

  /**
   * Exercise 5.4
   *
   * Implement forAll,
   * which checks that all elements in the Stream match a given predicate.
   * Your implementation should terminate the traversal
   * as soon as it encounters a nonmatching value.
   */
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)(p(_) && _)
  }

  /**
   * Exercise 5.5
   *
   * Use foldRight to implement takeWhile.
   */
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else Empty
    )
  }

  /**
   * Exercise 5.6 - Hard
   *
   * Implement headOption using foldRight.
   */
  def headOptionViaFoldRight: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  /**
   * Exercise 5.7
   *
   * Implement map, filter, append, and flatMap using foldRight.
   * The append method should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))
  }
  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => {
      if (p(a)) Stream.cons(a, b)
      else b
    })
  }
  /**
   * def append(as: => Stream[A]): Stream[A] = {
   *
   * http://stackoverflow.com/questions/9619121/why-is-parameter-in-contravariant-position
   */
  def append[B >: A](as: => Stream[B]): Stream[B] = {
    foldRight(as)(Stream.cons(_, _))
  }
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])(f(_).append(_))
  }

  /**
   * Exercise 5.13
   *
   * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3),
   * and zipAll.
   * The zipAll function should continue the traversal
   * as long as either stream has more elements
   * --- it uses Option to indicate whether each stream has been exhausted.
   */
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this)((s: Stream[A]) => {
      s match {
        case Empty => None
        case Cons(h, t) => Some(f(h()), t())
      }
    })
  }
  def takeViaUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n))((s: (Stream[A], Int)) => {
      s match {
        case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
        case _ => None
      }
    })
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold(this)((s: Stream[A]) => {
      s match {
        case Cons(h, t) if (p(h())) => Some(h(), t())
        case _ => None
      }
    })
  }
  def zipWithViaUnfold[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    Stream.unfold((this, bs))((s: (Stream[A], Stream[B])) => {
      s match {
        case (Cons(h, t), Cons(bh, bt)) => Some(f(h(), bh()), (t(), bt()))
        case _ => None
      }
    })
  }
  // FIXME: No test case provided yet.
  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    zipWithAll(s)((_, _))
  }
  def zipWithAll[B, C](bs: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    Stream.unfold((this, bs))((s: (Stream[A], Stream[B])) => {
      s match {
        case (Cons(h, t), Empty) => Some(f(Some(h()), None), (t(), Empty: Stream[B]))
        case (Empty, Cons(h, t)) => Some(f(None, Some(h())), (Empty: Stream[A], t()))
        case (Cons(h, t), Cons(bh, bt)) => Some(f(Some(h()), Some(bh())), (t(), bt()))
        case _ => None
      }
    })
  }

  /**
   * Exercise 5.14 - Hard
   *
   * Implement startsWith using functions you've written.
   * It should check if one Stream is a prefix of another.
   * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
   */
  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty).forAll(
      (pair: (Option[A], Option[B])) => pair match {case (h, h2) => h == h2}
    )
  }

  /**
   * Exercise 5.15
   * Implement tails using unfold.
   * For a given Stream, tails returns the Stream of suffixes of the input sequence,
   * starting with the original Stream.
   * For example, given Stream(1,2,3),
   * it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
   */
  def tails: Stream[Stream[A]] = {
    Stream.unfold(this)((s) => s match {
      case Empty => None
      case Cons(h, t) => Some(s, s.drop(1))
    }).append(Stream(Empty))
  }

  /**
   * Exercise 5.16 - Hard
   *
   * Generalize tails to the function scanRight,
   * which is like a foldRight
   * that returns a stream of the intermediate results.
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    Stream.unfold(this)((s) => s match {
      case Empty => None
      case s => Some(s.foldRight(z)(f), s.drop(1))
    }).append(Stream(z))
  }
  def scanRightViaFoldRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  /**
   * Exercise 5.8
   *
   * Generalize ones slightly to the function constant,
   * which returns an infinite Stream of a given value.
   */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
   * Exercise 5.9
   *
   * Write a function that generates an infinite stream of integers,
   * starting from n , then n + 1 , n + 2 , and so on.
   */
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  /**
   * Exercise 5.10
   *
   * Write a function fibs that generates the infinite stream of Fibonacci numbers:
   * 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  def fibs(a: Int, b: Int): Stream[Int] = {
    cons(a, cons(b, fibs(a + b, b + a + b)))
  }

  /**
   * Exercise 5.11
   *
   * Write a more general stream-building function called unfold.
   * It takes an initial state,
   * and a function for producing both the next state and the next value
   * in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  /**
   * Exercise 5.12
   *
   * Write fibs, from, constant, and ones in terms of unfold.
   */
  def fibsViaUnfold(a: Int, b: Int): Stream[Int] = {
    unfold((a, b))((s: (Int, Int)) => Some(s._1, (s._2, s._1 + s._2)))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(s => Some((s, s)))
  }

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(s => Some((s, s)))
  }
}
