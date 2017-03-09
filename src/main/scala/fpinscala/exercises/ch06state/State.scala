package fpinscala.exercise.ch06state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  /**
   * Exercise 6.1
   *
   * Write a function that uses RNG.nextInt
   * to generate a random integer between 0 and Int.maxValue (inclusive).
   * Make sure to handle the corner case when nextInt returns Int.MinValue,
   * which doesn’t have a non-negative counterpart.
   */

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt

    if (n < 0) (-(n + 1), nextRNG)
    else (n, nextRNG)
  }



  /**
   * Exercise 6.2
   *
   * Write a function to generate a Double between 0 and 1, not including 1.
   * Note: You can use Int.MaxValue to obtain the maximum positive integer value,
   * and you can use x.toDouble to convert an x: Int to a Double.
   *
   * FIXME: useful test cases not been provided.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, tempNextRNG) = rng.nextInt
    val (d, nextRNG) = double(tempNextRNG)
    ((n, d), nextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, tempNextRNG) = double(rng)
    val (n, nextRNG) = tempNextRNG.nextInt
    ((d, n), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, nextRNG) = double(rng2)
    ((d1, d2, d3), nextRNG)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (n == 0) (acc, rng)
      else {
        loop(n-1, rng.nextInt._2, (rng.nextInt._1 :: acc.reverse).reverse)
      }
    }
    loop(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }
  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  /**
   * Exercise 6.5
   *
   * Use map to reimplement double in a more elegant way.
   */
  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))
  }

  /**
   * Exercise 6.6
   *
   * Write the implementation of map2 based on the following signature.
   * This function takes two actions, ra and rb,
   * and a function f for combining their results,
   * and returns a new action that combines them
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (raa, rng1) = ra(rng)
      val (rba, rng2) = rb(rng1)
      (f(raa, rba), rng2)
    }
  }
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }
  def randIntDouble: Rand[(Int, Double)] = {
    both(int, double)
  }
  def randDoubleInt: Rand[(Double, Int)] = {
    both(double, int)
  }

  /**
   * Exercise 6.7 - Hard
   *
   * If you can combine two RNG transitions,
   * you should be able to combine a whole list of them.
   * Implement sequence for combining a List of transitions
   * into a single transition.
   * Use it to reimplement the ints function you wrote before.
   * For the latter, you can use the standard library function List.fill(n)(x)
   * to make a list with x repeated n times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      def loop(fs: List[Rand[A]], list: List[A], thisRNG: RNG): (List[A], RNG) = {
        fs match {
          case r::rs => loop(rs, (r(thisRNG)._1 :: list.reverse).reverse, r(thisRNG)._2)
          case _ => (list, thisRNG)
        }
      }
      loop(fs, List(), rng)
    }
  }
  def sequenceViaFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((rand, randOfList) => map2(rand, randOfList)(_ :: _))
  }
  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  /**
   * Exercise 6.8
   *
   * Implement flatMap, and then use it to implement nonNegativeLessThan.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { a =>
      val mod = a % n
      if (a + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  /**
   * Exercise 6.9
   *
   * Reimplement map and map2 in terms of flatMap.
   * The fact that this is possible is what we're referring to when we say that
   * flatMap is more powerful than map and map2.
   */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => { rng => {
      val (b, rngc) = rb(rng)
      (f(a, b), rngc)
    }})
  }
}

import State._
/**
 * Exercise 6.10
 *
 * Generalize the functions unit, map, map2, flatMap, and sequence.
 * Add them as methods on the State case class where possible.
 * Otherwise you should put them in a State companion object.
 */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State((s: S) => {
    val (a, ss) = run(s)
    (f(a), ss)
  })
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State((s: S) => {
    val (a, ss) = run(s)
    val (b, sss) = sb.run(ss)
    (f(a, b), sss)
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
    val (a, ss) = run(s)
    f(a).run(ss)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((s, sl) => {s.map2(sl)(_ :: _)})

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
 * Exercise 6.11 - Hard
 *
 * To gain experience with the use of State,
 * implement a finite state automaton that models a simple candy dispenser.
 * The machine has two types of input:
 * you can insert a coin, or you can turn the knob to dispense candy.
 * It can be in one of two states: locked or unlocked.
 * It also tracks how many candies are left and how many coins it contains.
 *
 * The rules of the machine are as follows:
 *  - Inserting a coin into a locked machine will cause it
 *    to unlock if there's any candy left.
 *  - Turning the knob on an unlocked machine will cause it
 *    to dispense candy and become locked.
 *  - Turning the knob on a locked machine
 *    or inserting a coin into an unlocked machine does nothing.
 *  - A machine that's out of candy ignores all inputs.
 */
object CandyDispenser {
  def stateMachine(machine: Machine, input: Input): Machine = (machine, input) match {
    case (Machine(_, candies, _), _) if candies <= 0 => machine
    case (Machine(true, candies, coins), Coin) => Machine(false, candies, coins + 1)
    case (Machine(true, _, _), Turn) => machine
    case (Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
    case (Machine(false, _, _), Coin) => machine
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def loop(inputs: List[Input], machine: Machine): Machine = inputs match {
      case x :: xs => loop(xs, stateMachine(machine, x))
      case _ => machine
    }

    def run = (machine: Machine) => {
      val nextMachine = loop(inputs, machine)
      ((nextMachine.coins, nextMachine.candies), nextMachine)
    }
    State(run)
  }
}
