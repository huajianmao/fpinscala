package fpinscala.exercises.ch09parsing

import scala.util.matching.Regex

import ReferenceTypes._


object ReferenceTypes {
  type Parser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {

  }

  sealed trait Result[+A] {

  }

  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}

object Reference extends Parsers[Parser] {
  def run[A](p: Parser[A])(s: String): Either[ParseError, A] = ???
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???
  def string(s: String): Parser[String] = ???
  def regex(r: Regex): Parser[String] = ???
  def slice[A]: Parser[String] = ???
}
