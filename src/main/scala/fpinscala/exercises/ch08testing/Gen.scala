package fpinscala.exercise.ch08testing

import fpinscala.exercises.ch05laziness.Stream
import fpinscala.exercises.ch06state._
import Prop._

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
   * Exercise 8.6
   *
   * Implement flatMap,
   * and then use it to implement this more dynamic version of listOfN.
   * Put flatMap and listOfN in the Gen class.
   */
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(this.sample.flatMap(f(_).sample))
  }
  def map[B](f: A => B): Gen[B] = {
    Gen(this.sample.map(f(_)))
  }
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(this.sample.map2(g.sample)(f))
  }
  def listOfN(size: Int): Gen[List[A]] = {
    Gen.listOfN(size, this)
  }
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(this.listOfN(_))
  }
}

object Gen {

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

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeLessThan(2)).map(_ % 2 == 0))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  /**
   * Exercise 8.7
   *
   * Implement union,
   * for combining two generators of the same type into one,
   * by pulling values from each generator with equal likelihood.
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(bool => if (bool) g1 else g2)

  /**
   * Exercise 8.8
   *
   * Implement weighted,
   * a version of union that accepts a weight for each Gen
   * and generates values from each Gen
   * with probability proportional to its weight.
   */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(State(RNG.double)).flatMap(d => {
      if (d < g1._2.abs / (g1._2.abs + g2._2.abs)) g1._1
      else g2._1
    })
  }
}

// trait Prop {
//   import Prop._
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

//   def check[A]: Either[(FailedCase, SuccessCount), SuccessCount]
// }

case class Prop(run: (TestCases, RNG) => Result) {
  /**
   * Exercise 8.9
   *
   * Now that we have a representation of Prop,
   * implement && and || for composing Prop values.
   * Notice that in the case of failure we don't know
   * which property was responsible,
   * the left or the right.
   * Can you devise a way of handling this,
   * perhaps by allowing Prop values to be assigned a tag or label
   * which gets displayed in the event of a failure?
   */
  def &&(p: Prop): Prop = Prop {
    (n, rng) => {
      val thisResult = this.run(n, rng)
      if (thisResult.isFalsified) thisResult
      else p.run(n, rng)
    }
  }
  def ||(p: Prop): Prop = Prop {
    (n, rng) => {
    val thisResult = this.run(n, rng)
      if (!thisResult.isFalsified) thisResult
      else p.run(n, rng)
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }
  def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
    s"generated and exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }
}

case class SGen[+A](g: Int => Gen[A])
