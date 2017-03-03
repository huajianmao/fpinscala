package fpinscala.exercises.ch04errorhandling

import scala.{Either => _}

sealed trait Either[+E, +A] {
  /**
   * Exercise 4.6
   *
   * Implement versions of map, flatMap, orElse, and map2 on Either
   * that operate on the Right value.
   */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)

  def map2Ugly[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(e), _) => Left(e)
    case (_, Left(e)) => Left(e)
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)
  }

  /**
   * Exercise 4.7
   *
   * Implement sequence and traverse for Either.
   * These should return the first error that's encountered, if there is one.
   */
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight(Right(Nil): Either[E, List[B]])((x, y) => f(x).map2(y)(_ :: _))
  }
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight(Right(Nil): Either[E, List[A]])((x, y) => x.map2(y)(_ :: _))
  }
  def sequenceViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }

  /**
   * Exercise 4.8
   * [FIXME: create a new trait in another file, and implement all the functions above]
   *
   * In this implementation, map2 is only able to report one error,
   * even if both the name and the age are invalid.
   * What would you need to change in order to report both errors?
   * Would you change map2 or the signature of mkPerson?
   * Or could you create a new data type that captures this requirement better than Either does,
   * with some additional structure?
   *
   * How would orElse , traverse , and sequence behave differently for that data type?
   */

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)
  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  }
  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  }
  def mkPerson(name: String, age: Int): Either[String, Person] = {
    mkName(name).map2(mkAge(age))(Person(_, _))
  }
  // Create a new data type would be better.
}
