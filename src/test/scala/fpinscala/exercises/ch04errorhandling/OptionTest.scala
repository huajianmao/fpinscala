package fpinscala.exercises.ch04errorhandling

import org.scalatest.FunSuite

/**
 * Created by hjmao on 17-3-2.
 */
class OptionTest extends FunSuite {

  test("testMap with None") {
    val me = None
    val f = ((a: Int) => a.toString)
    val expected = None
    val actual = me.map(f)
    assert(actual == expected)
  }

  test("testMap") {
    val me = Some(3)
    val f = ((a: Int) => a.toString)
    val expected = Some("3")
    val actual = me.map(f)
    assert(actual == expected)
  }

  test("testFlatMap with None") {
    val me = None
    val f = ((a: Int) => Some(a.toString))
    val expected = None
    val actual = me.flatMap(f)
    assert(actual == expected)
  }

  test("testFlatMap") {
    val me = Some(3)
    val f = ((a: Int) => Some(a.toString))
    val expected = Some("3")
    val actual = me.flatMap(f)
    assert(actual == expected)
  }

  test("testGetOrElse with None") {
    val me = Some("3")
    val default = "default"
    val expected = "3"
    val actual = me.getOrElse(default)
    assert(actual == expected)
  }

  test("testOrElse with None") {
    val me = None
    val default = "default"
    val expected = default
    val actual = me.getOrElse(default)
    assert(actual == expected)
  }

  test("testOrElse with Some") {
    val me = Some(3)
    val ob = Some(1)
    val expected = Some(3)
    val actual = me.orElse(ob)
    assert(actual == expected)
  }

  test("testFilter has None") {
    val me = Some(3)
    val f = ((a: Int) => a > 3)
    val expected = None
    val actual = me.filter(f)
    assert(actual == expected)
  }

  test("testFilter with true cond.") {
    val me = Some(4)
    val f = ((a: Int) => a > 3)
    val expected = Some(4)
    val actual = me.filter(f)
    assert(actual == expected)
  }

  test("testVariance with non-None value Seq") {
    val xs = Seq(1.0, 2, 3, 4, 5)
    val expected = Some(2)
    val actual = Option.variance(xs)
    assert(actual == expected)
  }

  test("testVariance with empty Seq") {
    val xs = Seq()
    val expected = None
    val actual = Option.variance(xs)
    assert(actual == expected)
  }

  test("testMap2 with a None") {
    val a = None
    val b = Some(1)
    val f = (a: Int, b: Int) => a / b
    val expected = None
    val actual = Option.map2(a, b)(f)
    assert(actual == expected)
  }

  test("testMap2 with b None") {
    val a = Some(1)
    val b = None
    val f = (a: Int, b: Int) => a / b
    val expected = None
    val actual = Option.map2(a, b)(f)
    assert(actual == expected)
  }

  test("testMap2 with non-None") {
    val a = Some(1)
    val b = Some(2)
    val f = (a: Int, b: Int) => 1.0 * a / b
    val expected = Some(0.5)
    val actual = Option.map2(a, b)(f)
    assert(actual == expected)
  }
  test("testMap2ViaFlatMap with a None") {
    val a = None
    val b = Some(1)
    val f = (a: Int, b: Int) => a / b
    val expected = None
    val actual = Option.map2ViaFlatMap(a, b)(f)
    assert(actual == expected)
  }

  test("testMap2ViaFlatMap with b None") {
    val a = Some(1)
    val b = None
    val f = (a: Int, b: Int) => a / b
    val expected = None
    val actual = Option.map2ViaFlatMap(a, b)(f)
    assert(actual == expected)
  }

  test("testMap2ViaFlatMap with non-None") {
    val a = Some(1)
    val b = Some(2)
    val f = (a: Int, b: Int) => 1.0 * a / b
    val expected = Some(0.5)
    val actual = Option.map2ViaFlatMap(a, b)(f)
    assert(actual == expected)
  }

  test("testSequence with good values") {
    val a = List(Some(1), Some(2), Some(3))
    val expected = Some(List(1, 2, 3))
    val actual = Option.sequence(a)
    assert(actual == expected)
  }

  test("testSequence with None value") {
    val a = List(Some(1), None, Some(3))
    val expected = None
    val actual = Option.sequence(a)
    assert(actual == expected)
  }

  test("testTraverse with good values") {
    val a = List(1, 2, 3)
    val f = (x:Int) => Some(x.toString)
    val expected = Some(List("1", "2", "3"))
    val actual = Option.traverse(a)(f)
    assert(actual == expected)
  }

  test("testTraverse with ok int values") {
    val a = List("1", "2", "3")
    val f = (x:String) => Option.Try(x.toInt)
    val expected = Some(List(1, 2, 3))
    val actual = Option.traverse(a)(f)
    assert(actual == expected)
  }

  test("testTraverse with bad value") {
    val a = List("1", "2", "x")
    val f = (x:String) => Option.Try(x.toInt)
    val expected = None
    val actual = Option.traverse(a)(f)
    assert(actual == expected)
  }
}
