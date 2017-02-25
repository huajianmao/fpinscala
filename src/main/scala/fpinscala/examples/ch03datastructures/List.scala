package fpinscala.examples.ch03datastructures

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

object List { // `List` companion object. Contains functions for creating and working with lists.

}
