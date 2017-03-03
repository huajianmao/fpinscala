package fpinscala.exercises.ch05laziness

import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("testToList with normal values") {
    val me = Stream(1, 2, 3, 4, 5)
    val expected = List(1, 2, 3, 4, 5)
    val actual = me.toList
    assert(actual == expected)
  }

  // FIXME: how to test if his is printed lazily
  test("testToList with lazy values") {
    val me = Stream({println("hi1"); 0 + 1},
                    {println("hi2"); 0 + 2},
                    {println("hi3"); 0 + 3},
                    {println("hi4"); 0 + 4})
    val expected = List(1, 2, 3, 4)
    println("after Stream and before toList")
    val actual = me.toList
    assert(actual == expected)
  }

  test("testToList with empty values") {
    val me = Empty
    val expected = Nil
    val actual = me.toList
    assert(actual == expected)
  }

  test("testTake with normal values") {
    val me = Stream(1, 2, 3, 4)
    val n = 2
    val expected = Stream(1, 2).toList
    val actual = me.take(n).toList
    assert(actual == expected)
  }

  test("testTake with normal expression values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val n = 2
    val expected = Stream(1, 2).toList
    val actual = me.take(n).toList
    assert(actual == expected)
  }

  test("testTake with Empty value") {
    val me = Empty
    val n = 2
    val expected = Empty
    val actual = me.take(n)
    assert(actual == expected)
  }

  test("testDrop with normal values") {
    val me = Stream(1, 2, 3, 4)
    val n = 2
    val expected = Stream(3, 4).toList
    val actual = me.drop(n).toList
    assert(actual == expected)
  }

  test("testDrop with normal expression values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val n = 2
    val expected = Stream(3, 4).toList
    val actual = me.drop(n).toList
    assert(actual == expected)
  }

  test("testDrop with Empty value") {
    val me = Empty
    val n = 2
    val expected = Empty
    val actual = me.drop(n)
    assert(actual == expected)
  }

  test("testTakeWhile with normal values") {
    val me = Stream(1, 2, 3, 4)
    val p = (x: Int) => x < 3
    val expected = Stream(1, 2).toList
    val actual = me.takeWhile(p).toList
    assert(actual == expected)
  }

  test("testTakeWhile with normal expression values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val p = (x: Int) => x < 3
    val expected = Stream(1, 2).toList
    val actual = me.takeWhile(p).toList
    assert(actual == expected)
  }

  test("testTakeWhile with Empty value") {
    val me = Empty
    val p = (x: Int) => x < 3
    val expected = Empty
    val actual = me.takeWhile(p)
    assert(actual == expected)
  }
}
