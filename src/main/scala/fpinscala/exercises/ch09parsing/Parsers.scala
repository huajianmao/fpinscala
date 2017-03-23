package fpinscala.exercises.ch09parsing

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def orString(s1: String, s2: String): Parser[String]

  val numA: Parser[Int] = char('a').many.map(_.size)
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  /**
   * EXERCISE 9.1
   *
   * Using product, implement the now-familiar combinator map2
   * and then use this to implement many1 in terms of many.
   * Note that we could have chosen to make map2 primitive
   * and defined product in terms of map2 as we've done in previous chapters.
   * The choice is up to you.
   */
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _)
  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
    p.product(p2).map(pair => f(pair._1, pair._2))
  }
  // char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_size)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = ???
    def map[B](f: A => B): Parser[B] = ???
    def slice: Parser[String] = ???

    def product[B](p: Parser[B]): Parser[(A, B)] = ???
    def **[B](p: Parser[B]): Parser[(A, B)] = product(p)
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
