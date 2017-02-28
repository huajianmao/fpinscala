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

  test("concatenate a list of lists with available values") {
    val listsList = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
    val expected = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val actual = List.concat(listsList)
    assert(actual == expected)
  }

  test("concatenate a list of lists with null values") {
    val listsList = List(List(1, 2, 3), Nil, List(7, 8, 9))
    val expected = List(1, 2, 3, 7, 8, 9)
    val actual = List.concat(listsList)
    assert(actual == expected)
  }

  test("concatenate a list of lists with all null values") {
    val listsList = List(Nil, Nil, Nil)
    val expected = Nil
    val actual = List.concat(listsList)
    assert(actual == expected)
  }

  test("transform a list of integers by adding 1") {
    val listsList = List(1, 2, 3, 4)
    val expected = List(2, 3, 4, 5)
    val actual = List.transform(listsList)
    assert(actual == expected)
  }

  test("transform Nil list by adding 1") {
    val listsList = Nil
    val expected = Nil
    val actual = List.transform(listsList)
    assert(actual == expected)
  }

  test("convert List of Double to list of String") {
    val listsList = List(1.0, 2.345, 5.93)
    val expected = List("1.0", "2.345", "5.93")
    val actual = List.convertToStringList(listsList)
    assert(actual == expected)
  }

  test("convert Nil List of Double to list of String") {
    val listsList = Nil
    val expected = Nil
    val actual = List.convertToStringList(listsList)
    assert(actual == expected)
  }

  test("map by adding") {
    val as = List(1, 2, 3, 4)
    val expected = List(2, 3, 4, 5)
    val actual = List.map(as)(_ + 1)
    assert(actual == expected)
  }

  test("map by transform") {
    val as = List(1, 2, 3, 4)
    val expected = List("1", "2", "3", "4")
    val actual = List.map(as)(_.toString)
    assert(actual == expected)
  }

  test("filter by larger than 2") {
    val as = List(1, 2, 3, 4)
    val expected = List(3, 4)
    val actual = List.filter(as)(_ > 2)
    assert(actual == expected)
  }

  test("filter by is not Nil") {
    val as = List(1, Nil, 3, 4)
    val expected = List(1, 3, 4)
    val actual = List.filter(as)(_ != Nil)
    assert(actual == expected)
  }

  test("flatMap by duplicate elements") {
    val as = List(1, 2, 3)
    val expected = List(1, 1, 2, 2, 3, 3)
    val actual = List.flatMap(as)(i => List(i, i))
    assert(actual == expected)
  }

  test("flatMap to implement filter") {
    val as = List(1, 2, 3)
    val expected = List(3)
    val actual = List.flatMap(as)(i => i match {
      case x if(x > 2) => List(x)
      case _ => Nil
    })
    assert(actual == expected)
  }

  test("filterViaFlatMap by larger than 2") {
    val as = List(1, 2, 3, 4)
    val expected = List(3, 4)
    val actual = List.filterViaFlatMap(as)(_ > 2)
    assert(actual == expected)
  }

  test("filterViaFlatMap by is not Nil") {
    val as = List(1, Nil, 3, 4)
    val expected = List(1, 3, 4)
    val actual = List.filterViaFlatMap(as)(_ != Nil)
    assert(actual == expected)
  }

  test("addTwoList with normal value") {
    val as = List(1, 2, 3)
    val bs = List(4, 5, 6)
    val expected = List(5, 7, 9)
    val actual = List.addTwoList(as, bs)
    assert(actual == expected)
  }

  test("addTwoList with one Nil") {
    val as = List(1, 2, 3)
    val bs = List()
    val expected = List(1, 2, 3)
    val actual = List.addTwoList(as, bs)
    assert(actual == expected)
  }

  test("addTwoList with two Nil") {
    val as = Nil
    val bs = List()
    val expected = Nil
    val actual = List.addTwoList(as, bs)
    assert(actual == expected)
  }

  test("zipWith with normal value") {
    val as = List(1, 2, 3)
    val bs = List(4, 5, 6)
    val expected = List(5, 7, 9)
    val actual = List.zipWith(as, bs, 0, 0)(_ + _)
    assert(actual == expected)
  }

  test("zipWith with one Nil") {
    val as = List(1, 2, 3)
    val bs = List()
    val expected = List(1, 2, 3)
    val actual = List.zipWith(as, bs, 0, 0)(_ + _)
    assert(actual == expected)
  }

  test("zipWith with two Nil") {
    val as = Nil
    val bs = List()
    val expected = Nil
    val actual = List.zipWith(as, bs, 0, 0)(_ + _)
    assert(actual == expected)
  }

  test("zipWith with string and int normal value") {
    val as = List("1", "2", "3")
    val bs = List(1, 3, 3)
    val expected = List(true, false, true)
    val actual = List.zipWith(as, bs, "", 0)(_ == _.toString)
    assert(actual == expected)
  }

  test("zipWith with string and int one Nil") {
    val as = List("1", "2", "3")
    val bs = List()
    val expected = List(false, false, false)
    val actual = List.zipWith(as, bs, "", 0)(_ == _.toString)
    assert(actual == expected)
  }

  test("zipWith with string and int two Nil") {
    val as = Nil
    val bs = List()
    val expected = Nil
    val actual = List.zipWith(as, bs, "", 0)(_ == _.toString)
    assert(actual == expected)
  }

  test("hasSubsequence with int") {
    val sup = List(1, 2, 3, 4, 5)
    val sub = List(2, 3)
    val expected = true
    val actual = List.hasSubsequence(sup, sub)
    assert(actual == expected)
  }

  test("hasSubsequence with long sub int") {
    val sup = List(1, 2, 3, 4, 5)
    val sub = List(2, 3, 4, 5, 6)
    val expected = false
    val actual = List.hasSubsequence(sup, sub)
    assert(actual == expected)
  }

  test("hasSubsequence with long sup Nil") {
    val sup = Nil
    val sub = List(2, 3, 4, 5, 6)
    val expected = false
    val actual = List.hasSubsequence(sup, sub)
    assert(actual == expected)
  }

  test("hasSubsequence with long sub Nil") {
    val sup = List(2, 3, 4, 5, 6)
    val sub = Nil
    val expected = true
    val actual = List.hasSubsequence(sup, sub)
    assert(actual == expected)
  }
}
