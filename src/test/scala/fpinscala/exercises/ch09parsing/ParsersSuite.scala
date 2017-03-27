package fpinscala.exercises.ch09parsing

import org.scalatest.FunSuite
import ReferenceTypes.Parser

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

  // val P = Reference

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
    // val json: Parser[JSON] = JSON.jsonParser(P)
    // val expected =
    // val actual = P.run(json)(jsonTxt)
    // assert(actual == expected)
  }
}
