package fpinscala.exercises.ch09parsing

/**
 * Exercise 9.9 - Hard
 *
 * At this point, you are going to take over the process.
 * You'll be creating a Parser[JSON] from scratch using the primitives we've defined.
 * You don't need to worry (yet) about the representation of Parser.
 * As you go, you'll undoubtedly discover additional combinators and idioms,
 * notice and factor out common patterns, and so on.
 * Use the skills you've been developing throughout this book, and have fun!
 * If you get stuck, you can always consult the answers.
 *
 * Here's some minimal guidance:
 *  - Any general-purpose combinators you discover can be added to the Parsers trait directly.
 *  - You'll probably want to introduce combinators
 *    that make it easier to parse the tokens of the JSON format (like string literals and numbers).
 *    For this you could use the regex primitive we introduced earlier.
 *    You could also add a few primitives like letter, digit, whitespace, and so on,
 *    for building up your token parsers.
 *
 * Consult the hints if you'd like more guidance.
 * A full JSON parser is given in the file JSON.scala in the answers.
 */
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def keyval = escapedQuoted ** (":" *> value)

    def array: Parser[JSON] = surround("[", "]")(
      value sep "," map (vs => JArray(vs.toIndexedSeq))
    ) scope "array"

    def obj: Parser[JSON] = surround("{", "}")(
      keyval sep "," map (kvs => JObject(kvs.toMap))
    ) scope "object"

    def lit: Parser[JSON] = scope("literal") {
      "null".as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      "true".as(JBool(true)) |
      "false". as(JBool(false))
    }

    def value: Parser[JSON] = lit | obj | array

    root(whitespace *> (obj | array))
  }
}

