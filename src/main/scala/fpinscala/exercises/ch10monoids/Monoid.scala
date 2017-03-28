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
}
