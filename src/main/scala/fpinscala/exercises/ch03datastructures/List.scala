package fpinscala.exercises.ch03datastructures

/**
 * Created by hjmao on 25/02/2017.
 */

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

/**
 * Another data constructor, representing nonempty lists.
 * Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
 * `List` companion object.
 * Contains functions for creating and working with lists.
 */
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) { Nil }
    else { Cons(as.head, apply(as.tail: _*)) }

  // Exercise 3.1
  // Answer: 3
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(h, xs)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) { l }
    else {
      l match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n-1)
      }
    }
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) { dropWhile(xs, f) }
      else { Cons(x, dropWhile(xs, f)) }
  }
  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] = {
    def loop(l: List[A], f: A => Boolean, acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(x, xs) =>
        if (f(x)) { loop(xs, f, acc) }
        else { loop(xs, f, Cons(x, acc)) }
    }
    def reverse(l: List[A]): List[A] = loop(l, (x: A) => true, Nil)
    reverse(loop(l, f, Nil))
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, tail) => Cons(x, init(tail))
  }

  // Exercise 3.7
  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)
  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  // Exercise 3.8
  // Answer: It will keep the List.
  //         exercise38(List(1, 2, 3) will return List(1, 2, 3)
  def exercise38[A](ns: List[A]): List[A] = foldRight(ns, Nil: List[A])(Cons(_, _))

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  // Exercise 3.10
  def foldLeftMine[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def loop(l: List[A], z: B, f: (B, A) => B, acc: B): B = l match {
      case Nil => acc
      case Cons(x, xs) => loop(xs, z, f, f(acc, x))
    }

    loop(l, z, f, z)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs))

  /**
   * Exercise 3.13 - Hard:
   * Can you write foldLeft in terms of foldRight?
   * How about the other way around?
   * Implementing foldRight via foldLeft is useful
   * because it lets us implement foldRight tail-recursively,
   * which means it works even for large lists without overflowing the stack.
   */
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(List.reverse(l), z)((a: A, b: B) => f(b, a))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(List.reverse(l), z)((b: B, a: A) => f(a, b))
}
