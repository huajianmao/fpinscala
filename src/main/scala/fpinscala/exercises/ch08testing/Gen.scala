package fpinscala.exercise.ch08testing

import fpinscala.exercise.ch06state._

/**
 * Exercise 8.1
 *
 * To get used to thinking about testing in this way,
 * come up with properties that specify the implementation of a sum:
 * List[Int] => Int function.
 * You don't have to write your properties down as executable ScalaCheck code
 * an informal description is fine.
 * Here are some ideas to get you started:
 *
 * 1. Reversing a list and summing it
 *    should give the same result as summing the original, nonreversed list.
 * 2. What should the sum be if all elements of the list are the same value?
 * 3. Can you think of other properties?
 */
// for 2, all the element should equals to the sum / list.length
// 4. multiple each of the List element
//    will get a sum of n * sum of the original List

/**
 * Exercise 8.2
 *
 * What properties specify a function that finds the maximum of a List[Int]?
 */
// 1. max should larger than all of the elements in the list.
// 2. max of the reverse list should equals to the max of the original list.

/**
sealed trait Gen[+A] {
  def listOf[A](a: Gen[A]): Gen[List[A]]
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}
*/
case class Gen[+A](sample: State[RNG, A]) {

  /**
   * Exercise 8.4
   *
   * Implement Gen.choose using this representation of Gen.
   * It should generate integers in the range start to stopExclusive.
   * Feel free to use functions you’ve already written.
   */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt).map(n =>
      start + n % (stopExclusive-start)
    ))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeLessThan(2)).map(_ % 2 == 0))
  }
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    ???
  }
}

trait Prop {
  import Prop._
/**
 * Exercise 8.3
 *
 * Assuming the following representation of Prop,
 * implement && as a method of Prop.
 *
 * trait Prop { def check: Boolean }
 */

/**
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
*/

  def check[A]: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}
