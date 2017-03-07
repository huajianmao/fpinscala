package fpinscala.exercise.ch06state;

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
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

