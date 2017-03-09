package fpinscala.exercise.ch06state;

import org.scalatest.FunSuite

class StateTest extends FunSuite {
  test("testNextInt") {
    val seed = 42
    val expected = (16159453, SimpleRNG(1059025964525L))
    val actual = SimpleRNG(42).nextInt

    assert(actual == expected)
  }

  test("testNonNegativeInt") {
    val rng = SimpleRNG(42)
    val expected = (16159453, SimpleRNG(1059025964525L))
    val actual = RNG.nonNegativeInt(rng)
    assert(actual == expected)
  }

  test("testDouble") {
    val rng = SimpleRNG(SimpleRNG(42).nextInt._1)
    val actual = RNG.double(rng)._1
    assert(actual < 1.0)
    assert(actual >= 0)
  }

  test("testDouble with map") {
    val doubles = (0 until 10000).map(i => {
      RNG.double(SimpleRNG(i))._1
    }).toList.forall(d => (d >= 0 && d < 1.0))
  }

  test("testIntDouble") {
    val rng = SimpleRNG(42)
    val expected = rng.nextInt._2.nextInt._2
    val actual = RNG.intDouble(rng)
    assert(actual._2 == expected)
    assert(actual._1._2 >= 0 && actual._1._2 < 1.0)
  }

  test("testInts") {
    val rng = SimpleRNG(42)
    val count = 100
    val actual = RNG.ints(count)(rng)
    assert(actual._1.length == count)
  }

  test("nonNegativeEven") {
    val rng = SimpleRNG(42)
    val actual = RNG.nonNegativeEven(rng)
    assert(actual._1 % 2 == 0 && actual._1 >= 0)
  }

  test("testDoubleViaMap") {
    val rng = SimpleRNG(SimpleRNG(42).nextInt._1)
    val actual = RNG.doubleViaMap(rng)._1
    assert(actual < 1.0)
    assert(actual >= 0)
  }

  test("testDoubleViaMap with map") {
    val doubles = (0 until 10000).map(i => {
      RNG.doubleViaMap(SimpleRNG(i))._1
    }).toList.forall(d => (d >= 0 && d < 1.0))
  }

  test("testSimulateMachine from book case") {
    val machine = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Turn)
    val expected= Machine(true, 1, 14)
    val (_, actual) = CandyDispenser.simulateMachine(inputs).run(machine)
    assert(actual == expected)
  }

  test("testSimulateMachine") {
    val machine = Machine(true, 9, 0)
    val inputs = List(Coin, Turn, Turn, Turn, Coin)
    val expected= Machine(false, 8, 2)
    val (_, actual) = CandyDispenser.simulateMachine(inputs).run(machine)
    assert(actual == expected)
  }

  test("testSimulateMachine with no candies") {
    val machine = Machine(true, 0, 0)
    val inputs = List(Coin, Turn, Turn, Turn, Coin)
    val expected= machine
    val (_, actual) = CandyDispenser.simulateMachine(inputs).run(machine)
    assert(actual == expected)
  }
}
