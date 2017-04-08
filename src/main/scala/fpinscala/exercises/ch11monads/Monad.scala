package fpinscala.exercises.ch11monads

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
    (map(fab)(_._1), map(fab)(_._2))
  }

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(a => unit(f(a)))
  }
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  /**
   * Exercise 11.3
   *
   * The sequence and traverse combinators should be pretty familiar to you by now,
   * and your implementations of them from various prior chapters
   * are probably all very similar.
   * Implement them once and for all on Monad[F].
   */
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List[A]()))((fa, acc) => map2(fa, acc)(_ :: _))
  }
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    sequence(la.map(f))
  }

  /**
   * Exercise 11.4
   * Implement replicateM.
   */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }

  /**
   * Exercise 11.5
   *
   * Think about how replicateM will behave for various choices of F.
   * For example, how does it behave in the List monad?
   * What about Option?
   * Describe in your own words the general meaning of replicateM.
   */
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  /**
   * Exercise 11.6 - Hard
   *
   * Here's an example of a function we haven't seen before.
   * Implement the function filterM.
   * It's a bit like filter, except that instead of a function from A => Boolean,
   * we have an A => F[Boolean].
   * (Replacing various ordinary functions like this
   * with the monadic equivalent often yields interesting results.)
   * Implement this function,
   * and then think about what it means for various data types.
   */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    map(sequence(ms.map(m => product(unit(m), f(m)))))(listab => listab.filter(_._2).map(_._1))
  }
}

object Monad {
  import fpinscala.exercises.ch08testing.Gen
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  /**
   * Exercise 11.1
   *
   * Write monad instances for Par, Parser, Option, Stream, and List.
   */
  import fpinscala.exercises.ch07parallelism.Par
  import fpinscala.exercises.ch07parallelism.Par._
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(pa)(f)
  }

  import fpinscala.exercises.ch04errorhandling._
  import scala.{Option => _}
  import scala.{Some => _}
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](sa: Stream[A])(f: A => Stream[B]): Stream[B] = sa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] = la.flatMap(f)
  }

  /**
   * Exercise 11.2 - Hard
   *
   * State looks like it would be a monad too,
   * but it takes two type arguments
   * and you need a type constructor of one argument to implement Monad.
   * Try to implement a State monad,
   * see what issues you run into,
   * and think about possible solutions.
   * We'll discuss the solution later in this chapter.
   */
  // TODO
  // val stateMonad =
}
