package fpinscala.exercises.ch09parsing

import org.scalatest.FunSuite
import ReferenceTypes.Parser
import fpinscala.exercises.ch09parsing.JSON._

class ParsersSuite extends FunSuite {
  test("firstNonMatchingIndex 1") {
    val s1 = "hello"
    val s2 = "world"
    val offset = 0
    val expected = 0
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  test("firstNonMatchingIndex 2") {
    val s1 = "hello"
    val s2 = "hello"
    val offset = 0
    val expected = -1
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  test("firstNonMatchingIndex 3") {
    val s1 = "hello"
    val s2 = ""
    val offset = 0
    val expected = -1
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  test("firstNonMatchingIndex 4") {
    val s1 = "hello"
    val s2 = "a"
    val offset = 0
    val expected = 0
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  test("firstNonMatchingIndex 5") {
    val s1 = "__hello"
    val s2 = "hello"
    val offset = 2
    val expected = -1
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  test("firstNonMatchingIndex 6") {
    val s1 = "__hello"
    val s2 = ""
    val offset = 2
    val expected = -1
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  test("firstNonMatchingIndex 7") {
    val s1 = "__hello"
    val s2 = "a"
    val offset = 2
    val expected = 0
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  test("firstNonMatchingIndex 8") {
    val s1 = "hello"
    val s2 = "hello"
    val offset = 9
    val expected = 0
    val actual = ReferenceTypes.firstNonMatchingIndex(s1, s2, offset)
    assert(actual == expected)
  }

  val P = Reference
  val json: Parser[JSON] = JSON.jsonParser(P)

  test("JSON parser with well formed content") {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""
    val expected = JObject(Map(
      "Company name" -> JString("Microsoft Corporation"),
      "Ticker" -> JString("MSFT"),
      "Active" -> JBool(true),
      "Price" -> JNumber(30.66),
      "Shares outstanding" -> JNumber(8.38e9),
      "Related companies" -> JArray(Vector(JString("HPQ"), JString("IBM"), JString("YHOO"),
                                           JString("DELL"), JString("GOOG")))
    ))
    val actual = P.run(json)(jsonTxt).right.get
    assert(actual == expected)
  }

  test("JSON parser with well malformedJson1") {
    val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""
    val expected = true
    val actual = P.run(json)(malformedJson1)

    assert(actual.isLeft == expected)
    assert(actual.left.get.stack.length == 2)
    assert(actual.left.get.stack(1)._1.offset == malformedJson1.indexOf(';'))
  }

  test("JSON parser with well malformedJson2") {
    val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""
    val expected = true
    val actual = P.run(json)(malformedJson2)

    assert(actual.isLeft == expected)
    assert(actual.left.get.stack.length == 3)
    assert(actual.left.get.stack(2)._1.offset == malformedJson2.indexOf("++"))
  }
}
