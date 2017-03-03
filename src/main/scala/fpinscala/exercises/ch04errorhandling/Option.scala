package fpinscala.exercises.ch04errorhandling

import scala.{Option => _}
import scala.{Either => _}

/**
 * Created by hjmao on 17-3-2.
 */

sealed trait Option[+A] {

  /**
   * Exercise 4.1
   * Implement all of the preceding functions on Option.
   * As you implement each function,
   * try to think about what it means and in what situations you'd use it.
   * We'll explore when to use each of these functions next.
   */
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case _ => this
    }
  }
  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /**
   * Exercise 4.2
   * Implement the variance function in terms of flatMap.
   * If the mean of a sequence is m,
   * the variance is the mean of math.pow(x - m, 2)
   * for each element x in the sequence.
   *
   * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
   */
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: (A => B)): Option[A] => Option[B] = _ map f
  def absO: Option[Double] => Option[Double] = lift(math.abs)

  /**
   * Exercise 4.3
   *
   * Write a generic function map2 that combines two Option values
   * using a binary function.
   * If either Option value is None, then the return value is too.
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  def map2ViaFlatMap[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap((x: A) => (b.map((y: B) => f(x, y))))
  }

  /**
   * Exercise 4.4
   *
   * Write a function sequence that combines a list of Option s
   * into one Option containing a list of all the Some values in the original list.
   * If the original list contains None even once,
   * the result of the function should be None;
   * otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])((x, y) => map2(x, y)(_::_))
  }

  /**
   * Exercise 4.5
   *
   * Implement this function.
   * It's straightforward to do using map and sequence,
   * but try for a more efficient implementation that only looks at the list once.
   * In fact, implement sequence in terms of traverse.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(Nil): Option[List[B]])((x, y) => map2(f(x), y)(_::_))
  }
  /**
   * Try is used for test
   */
  def Try[A](a: => A): Option[A] = {
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }
  }
}
