package fpinscala.exercises.ch03datastructures

import org.scalatest.FunSuite

/**
 * Created by hjmao on 28/02/2017.
 */
class TreeSuite extends FunSuite {
  test("testSize") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val expected = 5
    val actual = Tree.size(tree)
    assert(actual == expected)
  }

  test("testMaximum") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val expected = 3
    val actual = Tree.maximum(tree)
    assert(actual == expected)
  }

  test("testMaximum with one leaf") {
    val tree = Leaf(1)
    val expected = 1
    val actual = Tree.maximum(tree)
    assert(actual == expected)
  }

  test("testDepth") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val expected = 2
    val actual = Tree.depth(tree)
    assert(actual == expected)
  }

  test("testDepth with one leaf") {
    val tree = Leaf(1)
    val expected = 0
    val actual = Tree.depth(tree)
    assert(actual == expected)
  }

  test("testMap") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (a: Int) => a + 4
    val expected = Branch(Branch(Leaf(5), Leaf(6)), Leaf(7))
    val actual = Tree.map(tree)(f)
    assert(actual == expected)
  }

  test("testMap with to String") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (a: Int) => a.toString
    val expected = Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
    val actual = Tree.map(tree)(f)
    assert(actual == expected)
  }

  test("testFold by testSize") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val expected = 5
    val actual = Tree.fold(tree)(_ => 1)(1 + _ + _)
    assert(actual == expected)
  }

  test("testFold by testMaximum") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val expected = 3
    val actual = Tree.fold(tree)((a: Int) => a)(_ max _)
    assert(actual == expected)
  }

  test("testFold by testMaximum with one leaf") {
    val tree = Leaf(1)
    val expected = 1
    val actual = Tree.fold(tree)((a: Int) => a)(_ max _)
    assert(actual == expected)
  }

  test("testFold by testDepth") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val expected = 2
    val actual = Tree.fold(tree)(_ => 0)(1 + _ + _)
    assert(actual == expected)
  }

  test("testFold by testDepth with one leaf") {
    val tree = Leaf(1)
    val expected = 0
    val actual = Tree.fold(tree)(_ => 0)(1 + _ + _)
    assert(actual == expected)
  }

  test("testFold by testMap") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (a: Int) => a + 4
    val expected = Branch(Branch(Leaf(5), Leaf(6)), Leaf(7))
    val actual = Tree.mapViaFold(tree)(f)
    assert(actual == expected)
  }

  test("testFold by testMap with to String") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val f = (a: Int) => a.toString
    val expected = Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
    val actual = Tree.mapViaFold(tree)(f)
    assert(actual == expected)
  }

}
