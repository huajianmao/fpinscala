package fpinscala.exercises.ch08testing

import java.util.concurrent.{Executors, ExecutorService}

import fpinscala.exercises.ch05laziness.Stream
import fpinscala.exercises.ch06state._
import Gen._
import Prop._
import fpinscala.exercises.ch07parallelism.Par
import fpinscala.exercises.ch07parallelism.Par.Par

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

  /**
   * Exercise 8.10
   *
   * Implement helper functions for converting Gen to SGen.
   * You can add this as a method on Gen.
   */
  def unsized: SGen[A] = SGen(size => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = {
    (this map2 g) ((_, _))
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

  /**
   * Exercise 8.12
   *
   * Implement a listOf combinator that doesn't accept an explicit size.
   * It should return an SGen instead of a Gen.
   * The implementation should generate lists of the requested size.
   */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))

  /**
   * Exercise 8.13
   *
   * Define listOf1 for generating nonempty lists,
   * and then update your specification of max to use this generator.
   */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOfN(n max 1))
  }
  /**
   * val smallInt = Gen.choose(-10, 10)
   * val maxProp = forAll(listOf1(smallInt)) { ns =>
   *   val max = ns.max
   *   !ns.exists(_ > max)
   */

  /**
   * Exercise 8.14
   *
   * Write a property to verify the behavior of List.sorted (API docs link: http://mng.bz/ Pz86),
   * which you can use to sort (among other things) a List[Int].
   * For instance, List(2,1,3).sorted is equal to List(1,2,3).
   */
  /**
   * val range = Gen.choose(-1000, 1000)
   * val sortedProp = forAll(listOf1(range)) { ns =>
   *   val sorted = ns.sorted
   *   sorted.foldRight((true, Int.MinValue))((n, acc) => ((acc._1 && acc._2 <= n), n))._1
   * }
   */

  /**
   * Exercise 8.16 - Hard
   *
   * Write a richer generator for Par[Int],
   * which builds more deeply nested parallel computations
   * than the simple ones we gave previously.
   */
  /**
   * Copied from
   * https://github.com/fpinscala/fpinscala/blob
   *        /master/answers/src/main/scala/fpinscala/testing/Gen.scala#L281
   */
  val pint2: Gen[Par[Int]] = {
    choose(-100, 100).listOfN(choose(0, 20)).map(l =>
      l.foldLeft(Par.unit(0))((p, i) =>
        Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))
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

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
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
    (max, n, rng) => {
      val thisResult = this.run(max, n, rng)
      if (thisResult.isFalsified) thisResult
      else p.run(max, n, rng)
    }
  }
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => {
      val thisResult = this.run(max, n, rng)
      if (!thisResult.isFalsified) thisResult
      else p.run(max, n, rng)
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
  case object Proved extends Result {
    def isFalsified: Boolean = false
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
    (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0)
                                      .take((n min max) + 1)
                                      .map(i => forAll(g(i))(f))
      props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) })
           .toList
           .reduce(_ && _).run(max, n, rng)
  }
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
    forAll(n => g(n))(f)
  }

  // scalastyle:off println
  def run(p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
  }
  // scalastyle:on println
  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  val p2 = Prop.check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = {
    Par.map2(p, p2)(_ == _)
  }

  val p3 = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES).get
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    forAll(S ** g) { case (s, a) => f(a)(s).get }
  }

  def checkPar[A](p: Par[Boolean]): Prop = {
    forAllPar(Gen.unit(()))(_ => p)
  }

  val p2ViaCheckPar = checkPar {
    equal (
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint: Gen[Par[Int]] = Gen.choose(0, 10) map (Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  /**
   * Exercise 8.17
   *
   * Express the property about fork from chapter 7, that fork(x) == x.
   */
  val p5 = forAllPar(pint)((n: Par[Int]) => equal(Par.fork(n), n))

  /**
   * Exercise 8.18
   *
   * Come up with some other properties that takeWhile should satisfy.
   * Can you think of a good property expressing the relationship
   * between takeWhile and dropWhile?
   */
  val s = List(1, 2, 3)
  val f = (x: Int) => x > 2
  val p6 = s.takeWhile(f) ::: s.dropWhile(f) == s
}

case class SGen[+A](g: Int => Gen[A]) {
  /**
   * Exercise 8.11
   *
   * Not surprisingly,
   * SGen at a minimum supports many of the same operations as Gen,
   * and the implementations are rather mechanical.
   * Define some convenience functions on SGen
   * that simply delegate to the corresponding functions on Gen.
   */
  def apply(n: Int): Gen[A] = g(n)
  def map[B](f: A => B): SGen[B] = SGen(apply(_).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n =>
    apply(n).flatMap(f(_).apply(n))
  )
}
