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

}
