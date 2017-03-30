package fpinscala.exercises.ch10monoids

import fpinscala.exercises.ch08testing.{Gen, Prop}
import Gen._
import Prop._

trait Monoid[A] {
  def op(a1: A, as: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = Nil
  }

  /**
   * Exercise 10.1
   *
   * Give Monoid instances for integer addition and multiplication
   * as well as the Boolean operators.
   */
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero: Boolean = true
  }

  /**
   * Exercise 10.2
   *
   * Give a Monoid instance for combining Option values.
   */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 match {
      case Some(b) => a1
      case _ => a2
    }
    def zero: Option[A] = None
  }

  /**
   * Exercise 10.3
   *
   * A function having the same argument and return type
   * is sometimes called an endofunction.
   * Write a monoid for endofunctions.
   */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f compose g
    def zero: A => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  /**
   * Exercise 10.4
   *
   * Use the property-based testing framework we developed in part 2
   * to implement a property for the monoid laws.
   * Use your property to test the monoids we've written.
   */
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(Gen.listOfN(3, gen))(list =>
  {
    val v1 = list(1)
    val v2 = list(2)
    val v3 = list(3)
    m.op(m.op(v1, v2), v3) == m.op(v1, m.op(v2, v3)) &&
    m.op(v1, m.zero) == v1 &&
    m.op(v2, m.zero) == v2 &&
    m.op(v3, m.zero) == v3
  })

  /**
   * Exercise 10.5
   *
   * Implement foldMap.
   */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  /**
   * Exercise 10.6 - Hard
   *
   * The foldMap function can be implemented using either foldLeft or foldRight.
   * But you can also write foldLeft and foldRight using foldMap! Try it.
   */
  /**
   * Copied from
   * @URL fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/monoids/Monoid.scala#L98
   */
  def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(a => {b => f(a, b)})(z)
  }
  def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => {b => f(b, a)})(z)
  }

  /**
   * Exercise 10.7
   *
   * Implement a foldMap for IndexedSeq.
   * Your implementation should use the strategy of splitting the sequence in two,
   * recursively processing each half,
   * and then adding the answers together with the monoid.
   */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) m.zero
    else {
      val middle = (0 + v.length) / 2
      m.op(foldMapV(v.slice(0, middle), m)(f), foldMapV(v.slice(middle, v.length), m)(f))
    }
  }

  /**
   * Exercise 10.8 - Hard
   *
   * Also implement a parallel version of foldMap using the library we developed in chapter 7.
   *
   * Hint: Implement par, a combinator to promote Monoid[A] to a Monoid[Par[A]],
   * and then use this to implement parFoldMap.
   */
  import fpinscala.exercises.ch07parallelism.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = es => {
      unit(m.op(a1(es).get, a2(es).get))(es)
    }
    def zero: Par[A] = unit(m.zero)
  }
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parM: Monoid[Par[B]] = par(m)
    if (v.length == 0) parM.zero
    else {
      val middle = (0 + v.length) / 2
      parM.op(parFoldMap(v.slice(0, middle), m)(f),
              parFoldMap(v.slice(middle, v.length), m)(f)
      )
    }
  }

  /**
   * Exercise 10.9 - Hard
   *
   * Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You'll need
   * to come up with a creative Monoid.
   */
  def isIndexedSeqInOrder(v: IndexedSeq[Int]): Boolean = {
    if (v.length == 0) true
    else {
      val pairList = v.zipAll(v.tail, Int.MinValue, Int.MaxValue)
      foldMap(pairList.toList, booleanAnd)(pair => pair._1 <= pair._2)
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  /**
   * Exercise 10.10
   *
   * Write a monoid instance for WC and make sure that it meets the monoid laws.
   */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(w1: WC, w2: WC): WC = (w1, w2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l2, n2, r2)) => Part(a + l2, n2, r2)
      case (Part(l1, n1, r1), Stub(b)) => Part(l1, n1, r1 + b)
      case (Part(l1, n1, r1), Part(l2, n2, r2)) =>
        Part(l1,
             n1 + n2 + (if ((r1 + l2).isEmpty) 0 else 1),
             r2)
    }
    def zero: WC = Stub("")
  }

  /**
   * Exercise 10.11
   *
   * Use the WC monoid to implement a function that counts words in a String
   * by recursively splitting it into substrings
   * and counting the words in those substrings.
   *
   * Copied from
   * @URL fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/monoids/Monoid.scala
   */
  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
}
