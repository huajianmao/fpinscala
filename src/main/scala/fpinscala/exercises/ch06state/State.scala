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

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
