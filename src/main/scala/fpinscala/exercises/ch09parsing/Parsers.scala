package fpinscala.exercises.ch09parsing

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def succeed[A](a: A): Parser[A]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]

  def defaultSucceed[A](a: A): Parser[A] = string("") map (_ => a)
  /**
   * Exercise 9.7
   *
   * Implement product and map2 in terms of flatMap.
   */
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = {
    flatMap(p1)(a => map(p2)((a, _)))
  }
  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    flatMap(p)(a => map(p2)(f(a, _)))
  }
  /**
   * Exercise 9.6
   *
   * Using flatMap and any other combinators,
   * write the context-sensitive parser we couldn't express earlier.
   * To parse the digits, you can make use of a new primitive,
   * regex, which promotes a regular expression to a Parser.
   * In Scala, a string s can be promoted to a Regex object
   * (which has methods for matching) using s.r,
   * for instance, "[a-zA-Z_][a-zA-Z0-9_]*".r.
   * implicit def regex(r: Regex): Parser[String]
   */

  /**
   * Exercise 9.8
   *
   * map is no longer primitive.
   * Express it in terms of flatMap and/or other combinators.
   */
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
    flatMap(p)(a => succeed(f(a)))
  }
  def orString(s1: String, s2: String): Parser[String] = or(succeed(s1), succeed(s2))
  val numA: Parser[Int] = many(char('a')).map(_.size)
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  /**
   * EXERCISE 9.1
   *
   * Using product, implement the now-familiar combinator map2
   * and then use this to implement many1 in terms of many.
   * Note that we could have chosen to make map2 primitive
   * and defined product in terms of map2 as we've done in previous chapters.
   * The choice is up to you.
   */
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    p.product(p2).map(pair => f(pair._1, pair._2))
  }

  /**
   * Exercise 9.3 - Hard
   *
   * Before continuing, see if you can define many in terms of or, map2, and succeed.
   */
  def many[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _) or self.succeed(Nil: List[A])
    // map2(p, wrap(many(p)))(_ :: _) or self.succeed(Nil: List[A])
  }

  /**
   * Exercise 9.5
   *
   * We could also deal with non-strictness with a separate combinator
   * like we did in chapter 7.
   * Try this here and make the necessary changes to your existing combinators.
   * What do you think of that approach in this instance?
   */
  // def wrap[A](p: => Parser[A]): Parser[A]


  // char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_size)

  /**
   * Exercise 9.4 - Hard
   *
   * Using map2 and succeed, implement the listOfN combinator from earlier.
   */
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)
  }

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def slice: Parser[String] = self.slice(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = p.product(p2)
  }

  import fpinscala.exercises.ch08testing._
  import fpinscala.exercises.ch08testing.Prop._
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    }
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      equal(p, p.map(a => a))(in)
    }
    def succeedLaw[A](a: A)(in: Gen[String]): Prop = {
      forAll(in)(s => run(succeed(a))(s) == Right(a))
    }

    /**
     * EXERCISE 9.2 - Hard
     *
     * Copied from
     * https://github.com/fpinscala/fpinscala/blob/master/answerkey/parsing/02.answer.scala
     *
     * Try coming up with laws to specify the behavior of product.
     */
    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
    def productRaw[A, B, C, D, E](a: Parser[A], b: Parser[B], c: Parser[C])
                                 (f: A => D, g: B => E)
                                 (in: Gen[String]): Prop = {
      equal((a ** b) ** c map unbiasL, a ** (b ** c) map unbiasR)(in) &&
      equal((a ** b).map(pair => (f(pair._1), g(pair._2))), a.map(f) ** b.map(g))(in)
    }
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))
  def advanceBy(n: Int): Location = copy(offset = offset + n)

  def currentLine: String = {
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
  }
  def columnCaret: String = (" " * (col - 1)) + "^"
}

case class ParseError(stack: List[(Location, String)] = List()) {
  def push(loc: Location, msg: String): ParseError = copy(stack = ((loc, msg) :: stack))
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
  def latest: Option[(Location, String)] = stack.lastOption
  def latestLoc: Option[Location] = latest.map(_._1)
  def formatLoc(l: Location): String = l.line + "." + l.col

  override def toString: String = {
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context = collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
                    collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }
               .mkString("\n") + context
    }
  }
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] = {
    s.groupBy(_._1)
     .mapValues(_.map(_._2).mkString("; "))
     .toList.sortBy(_._1.offset)
  }
}

object Parsers {

}
