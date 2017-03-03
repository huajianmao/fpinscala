package fpinscala.exercises.ch04errorhandling

import org.scalatest.FunSuite

/**
 * Created by hjmao on 17-3-3.
 */
class EitherTest extends FunSuite {
  test("testMap with right value") {
    val me = Right(1)
    val f = (x: Int) => x + 1
    val expected = Right(2)
    val actual = me.map(f)
    assert(actual == expected)
  }

  test("testMap with error value") {
    val me = Left("Error")
    val f = (x: Int) => x + 1
    val expected = Left("Error")
    val actual = me.map(f)
    assert(actual == expected)
  }

  test("testFlatMap with right value") {
    val me = Right(1)
    val f = (x: Int) => Right(x + 1)
    val expected = Right(2)
    val actual = me.flatMap(f)
    assert(actual == expected)
  }

  test("testFlatMap with error value") {
    val me = Left("Error")
    val f = (x: Int) => Right(x + 1)
    val expected = Left("Error")
    val actual = me.flatMap(f)
    assert(actual == expected)
  }

  test("testOrElse with right value") {
    val me = Right(1)
    val b = Right(2)
    val expected = Right(1)
    val actual = me.orElse(b)
    assert(actual == expected)
  }

  test("testOrElse with error value") {
    val me = Left("Error")
    val b = Right(1)
    val expected = Right(1)
    val actual = me.orElse(b)
    assert(actual == expected)
  }

  test("testOrElse with two error values") {
    val me = Left("Error")
    val b = Left(1)
    val expected = Left(1)
    val actual = me.orElse(b)
    assert(actual == expected)
  }

  test("testMap2 with right values") {
    val me = Right(1)
    val b = Right(2)
    val f = (a: Int, b: Int) => a + b
    val expected = Right(3)
    val actual = me.map2(b)(f)
    assert(actual == expected)
  }

  test("testMap2 with one error value") {
    val me = Right(1)
    val b = Left(2)
    val f = (a: Int, b: Int) => a + b
    val expected = Left(2)
    val actual = me.map2(b)(f)
    assert(actual == expected)
  }

  test("testMap2 with two error values") {
    val me = Left(1)
    val b = Left(2)
    val f = (a: Int, b: Int) => a + b
    val expected = Left(1)
    val actual = me.map2(b)(f)
    assert(actual == expected)
  }

  test("testTraverse with right values") {
    val as = List(1, 2, 3, 4)
    val f = (x: Int) => Right(x + 1)
    val expected = Right(List(2, 3, 4, 5))
    val actual = Either.traverse(as)(f)
    assert(actual == expected)
  }

  test("testTraverse with string values") {
    val as = List("1", "2", "3", "4")
    val f = (x: String) => Either.Try(x.toInt + 1)
    val expected = Right(List(2, 3, 4, 5))
    val actual = Either.traverse(as)(f)
    assert(actual == expected)
  }

  test("testTraverse with error string value") {
    val as = List("x", "2", "y", "4")
    val f = (x: String) => Either.Try(x.toInt + 1)
    val expected = Either.Try("x".toInt).toString
    val actual = Either.traverse(as)(f).toString
    assert(actual == expected)
  }
}
