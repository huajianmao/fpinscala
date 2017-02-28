package fpinscala.exercises.ch03datastructures

import org.scalatest.FunSuite

/**
 * Created by hjmao on 26/02/2017.
 */
class ListSuite extends FunSuite {

  test("testReverse") {
    val l = List(1, 2, 3, 4)
    val expected = List(4, 3, 2, 1)
    assert(List.reverse(l) == expected)
  }

  test("foldLeftViaFoldRight with sum") {
    val l = List(1, 2, 3, 4)
    val expected = 10
    val actual = List.foldLeftViaFoldRight(l, 0)(_ + _)
    assert(actual == expected)
  }

  test("foldLeftViaFoldRight with divide") {
    val l = List(24, 3, 2, 1)
    val expected = 1
    val actual = List.foldLeftViaFoldRight(l, 144)(_ / _)
    assert(actual == expected)
  }

  test("foldRightViaFoldLeft with sum") {
    val l = List(1, 2, 3, 4)
    val expected = 10
    val actual = List.foldRightViaFoldLeft(l, 0)(_ + _)
    assert(actual == expected)
  }

  test("foldRightViaFoldLeft with divide") {
    val l = List(24, 3, 2, 1)
    val expected = 16
    val actual = List.foldRightViaFoldLeft(l, 1.0)(_ / _)
    assert(actual == expected)
  }

  test("appendViaFoldRight with available value") {
    val head = List(1, 2, 3, 4)
    val tail = List(5, 6, 7, 8)
    val expected = List(1, 2, 3, 4, 5, 6, 7, 8)
    val actual = List.appendViaFoldRight(head, tail)
    assert(actual == expected)
  }

  test("appendViaFoldRight with head null") {
    val head = Nil
    val tail = List(5, 6, 7, 8)
    val expected = List(5, 6, 7, 8)
    val actual = List.appendViaFoldRight(head, tail)
  }

  test("appendViaFoldRight with tail null") {
    val head = List(5, 6, 7, 8)
    val tail = Nil
    val expected = List(5, 6, 7, 8)
    val actual = List.appendViaFoldRight(head, tail)
  }

  test("appendViaFoldLeft with available value") {
    val head = List(1, 2, 3, 4)
    val tail = List(5, 6, 7, 8)
    val expected = List(1, 2, 3, 4, 5, 6, 7, 8)
    val actual = List.appendViaFoldRight(head, tail)
    assert(actual == expected)
  }

  test("appendViaFoldLeft with head null") {
    val head = Nil
    val tail = List(5, 6, 7, 8)
    val expected = List(5, 6, 7, 8)
    val actual = List.appendViaFoldRight(head, tail)
    assert(actual == expected)
  }

  test("appendViaFoldLeft with tail null") {
    val head = List(5, 6, 7, 8)
    val tail = Nil
    val expected = List(5, 6, 7, 8)
    val actual = List.appendViaFoldRight(head, tail)
    assert(actual == expected)
  }
}
