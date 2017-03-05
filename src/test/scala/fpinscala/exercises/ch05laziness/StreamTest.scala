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

  test("testForAll with all true values") {
    val me = Stream(1, 2, 3, 4)
    val p = (x: Int) => x > 0
    val expected = true
    val actual = me.forAll(p)
    assert(actual == expected)
  }

  test("testForAll with all true string values") {
    val me = Stream("x1", "x2", "x3", "x4")
    val p = (x: String) => x.contains("x")
    val expected = true
    val actual = me.forAll(p)
    assert(actual == expected)
  }

  test("testForAll with all false string values") {
    val me = Stream("x11", "x12", {println("x3"); "x3"}, "x4")
    val p = (x: String) => x.contains("x1")
    val expected = false
    val actual = me.forAll(p)
    assert(actual == expected)
  }

  test("testForAll with Empty") {
    val me = Empty
    val p = (x: String) => x.contains("x1")
    val expected = true
    val actual = me.forAll(p)
    assert(actual == expected)
  }

  test("testTakeWhileViaFoldRight with normal values") {
    val me = Stream(1, 2, 3, 4)
    val p = (x: Int) => x < 3
    val expected = Stream(1, 2).toList
    val actual = me.takeWhileViaFoldRight(p).toList
    assert(actual == expected)
  }

  test("testTakeWhileViaFoldRight with normal expression values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val p = (x: Int) => x < 3
    val expected = Stream(1, 2).toList
    val actual = me.takeWhileViaFoldRight(p).toList
    assert(actual == expected)
  }

  test("testTakeWhileViaFoldRight with Empty value") {
    val me = Empty
    val p = (x: Int) => x < 3
    val expected = Empty
    val actual = me.takeWhileViaFoldRight(p)
    assert(actual == expected)
  }

  test("testHeadOptionViaFoldRight with OK") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val expected = Some(1)
    val actual = me.headOptionViaFoldRight
    assert(actual == expected)
  }

  test("testHeadOptionViaFoldRight with Empty") {
    val me = Empty
    val expected = None
    val actual = me.headOptionViaFoldRight
    assert(actual == expected)
  }

  test("testMapViaFoldRight with normal values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val f = (x: Int) => x + 1
    val expected = Stream(2, 3, 4, 5).toList
    val actual = me.map(f).toList
    assert(actual == expected)
  }

  test("testMapViaFoldRight with Empty") {
    val me = Empty
    val f = (x: Int) => x + 1
    val expected = Empty
    val actual = me.map(f)
    assert(actual == expected)
  }

  test("testFilterViaFoldRight with normal values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val p = (x: Int) => x % 2 == 0
    val expected = Stream(2, 4).toList
    val actual = me.filter(p).toList
    assert(actual == expected)
  }

  test("testFilterViaFoldRight with Empty") {
    val me = Empty
    val p = (x: Int) => x % 2 == 0
    val expected = Empty
    val actual = me.filter(p)
    assert(actual == expected)
  }

  test("testAppendViaFoldRight with normal values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val as = Stream({println("hi"); 5}, {println("hi"); 6}, 7, 8)
    val expected = Stream(1, 2, 3, 4, 5, 6, 7, 8).toList
    val actual = me.append(as).toList
    assert(actual == expected)
  }

  test("testAppendViaFoldRight with Empty") {
    val me = Empty
    val as = Stream({println("hi"); 5}, {println("hi"); 6}, 7, 8)
    val expected = Stream(5, 6, 7, 8).toList
    val actual = me.append(as).toList
    assert(actual == expected)
  }

  test("testFlatMapViaFoldRight with normal values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val f = (x: Int) => Cons(() => x, () => Empty)
    val expected = Stream(1, 2, 3, 4).toList
    val actual = me.flatMap(f).toList
    assert(actual == expected)
  }

  test("testFlatMapViaFoldRight with Empty") {
    val me = Empty
    val f = (x: Int) => Cons(() => x, () => Empty)
    val expected = Empty
    val actual = me.flatMap(f)
    assert(actual == expected)
  }

  // FIXME: How to test infinite Stream data?
  test("testConstant with take") {
    val me = Stream.constant(1)
    val expected = List(1, 1, 1, 1)
    val actual = me.take(4).toList
    assert(actual == expected)
  }

  test("testFrom with take") {
    val me = Stream.from(4)
    val expected = List(4, 5, 6, 7)
    val actual = me.take(4).toList
    assert(actual == expected)
  }

  test("testFibs with take") {
    val me = Stream.fibs(0, 1)
    val expected = List(0, 1, 1, 2, 3, 5, 8)
    val actual = me.take(7).toList
    assert(actual == expected)
  }

  test("testUnfold with fibs") {
    val f = (s:(Int, Int)) => Some((s._1, (s._2, s._1 + s._2)))
    val me = Stream.unfold((0, 1))(f)
    val expected = List(0, 1, 1, 2, 3, 5, 8)
    val actual = me.take(7).toList
    assert(actual == expected)
  }

  test("testFibsViaUnfold with take") {
    val me = Stream.fibsViaUnfold(0, 1)
    val expected = List(0, 1, 1, 2, 3, 5, 8)
    val actual = me.take(7).toList
    assert(actual == expected)
  }

  test("testFromViaUnfold with take") {
    val me = Stream.fromViaUnfold(4)
    val expected = List(4, 5, 6, 7)
    val actual = me.take(4).toList
    assert(actual == expected)
  }

  test("testConstantViaUnfold with take") {
    val me = Stream.constantViaUnfold(1)
    val expected = List(1, 1, 1, 1)
    val actual = me.take(4).toList
    assert(actual == expected)
  }

  test("testOnesViaUnfold with take") {
    val me = Stream.onesViaUnfold
    val expected = List(1, 1, 1, 1)
    val actual = me.take(4).toList
    assert(actual == expected)
  }

  test("testMapViaUnfold with normal values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val f = (x: Int) => x + 1
    val expected = Stream(2, 3, 4, 5).toList
    val actual = me.mapViaUnfold(f).toList
    assert(actual == expected)
  }

  test("testMapViaUnfold with Empty") {
    val me = Empty
    val f = (x: Int) => x + 1
    val expected = Empty
    val actual = me.mapViaUnfold(f)
    assert(actual == expected)
  }

  test("testTakeViaUnfold with normal values") {
    val me = Stream(1, 2, 3, 4)
    val n = 2
    val expected = Stream(1, 2).toList
    val actual = me.takeViaUnfold(n).toList
    assert(actual == expected)
  }

  test("testTakeViaUnfold with normal expression values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val n = 2
    val expected = Stream(1, 2).toList
    val actual = me.takeViaUnfold(n).toList
    assert(actual == expected)
  }

  test("testTakeViaUnfold with Empty value") {
    val me = Empty
    val n = 2
    val expected = Empty
    val actual = me.takeViaUnfold(n)
    assert(actual == expected)
  }

  test("testTakeWhileViaUnfold with normal values") {
    val me = Stream(1, 2, 3, 4)
    val p = (x: Int) => x < 3
    val expected = Stream(1, 2).toList
    val actual = me.takeWhileViaUnfold(p).toList
    assert(actual == expected)
  }

  test("testTakeWhileViaUnfold with normal expression values") {
    val me = Stream({println("hi"); 1}, {println("hi"); 2}, 3, 4)
    val p = (x: Int) => x < 3
    val expected = Stream(1, 2).toList
    val actual = me.takeWhileViaUnfold(p).toList
    assert(actual == expected)
  }

  test("testTakeWhileViaUnfold with Empty value") {
    val me = Empty
    val p = (x: Int) => x < 3
    val expected = Empty
    val actual = me.takeWhileViaUnfold(p)
    assert(actual == expected)
  }

  test("testZipWith with normal value") {
    val as = Stream(1, 2, 3)
    val bs = Stream(4, 5, 6)
    val expected = Stream(5, 7, 9).toList
    val actual = as.zipWithViaUnfold(bs)((x: Int, y: Int) => x + y).toList
    assert(actual == expected)
  }

  test("testZipWith with one Empty") {
    val as = Stream(1, 2, 3)
    val bs = Stream()
    val expected = Empty
    val actual = as.zipWithViaUnfold(bs)((x: Int, y: Int) => x + y)
    assert(actual == expected)
  }

  test("testZipWith with two Empty") {
    val as = Empty
    val bs = Stream()
    val expected = Empty
    val actual = as.zipWithViaUnfold(bs)((x: Int, y: Int) => x + y)
    assert(actual == expected)
  }

  test("testZipWith with string and int normal value") {
    val as = Stream("1", "2", "3")
    val bs = Stream(1, 3, 3)
    val expected = Stream(true, false, true).toList
    val actual = as.zipWithViaUnfold(bs)((x: String, y: Int) => x == y.toString).toList
    assert(actual == expected)
  }

  test("testZipWith with string and int one Nil") {
    val as = Stream("1", "2", "3")
    val bs = Empty: Stream[Int]
    val expected = Empty
    val actual = as.zipWithViaUnfold(bs)((x: String, y: Int) => x == y.toString)
    assert(actual == expected)
  }

  test("testZipWith with string and int two Nil") {
    val as = Empty
    val bs = Stream()
    val expected = Nil
    val actual = as.zipWithViaUnfold(bs)((x: Int, y: Int) => x + y).toList
    assert(actual == expected)
  }


}
