package fpinscala.exercises.ch09parsing

import org.scalatest.FunSuite
import ReferenceTypes.Parser

class ParsersSuite extends FunSuite {
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
