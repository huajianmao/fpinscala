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
}
