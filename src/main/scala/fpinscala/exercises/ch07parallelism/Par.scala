package fpinscala.exercises.ch07parallelism

import java.util.concurrent._

import language.implicitConversions

object Par {

  // scalastyle:off noimpl
  /**
   * For exercise 7.1 -- start
   */
  // type Par[A] = Nothing // FIXME:
  // def unit[A](a: => A): Par[A] = { ??? }
  // def get[A](par: Par[A]): A = { ??? }

  /**
   * Exercise 7.1
   *
   * Par.map2 is a new higher-order function
   * for combining the result of two parallel computations.
   * What is its signature?
   * Give the most general signature possible
   * (don't assume it works only for Int).
   */
  // def map2[A, B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C] = {
  //   ???
  // }


  // def sum(ints: IndexedSeq[Int]): Par[Int] = {
  //   if (ints.size <= 1) {
  //     Par.unit(ints.headOption.getOrElse(0))
  //   } else {
  //     val (l, r) = ints.splitAt(ints.length / 2)
  //     Par.map2(sum(l), sum(r))(_ + _)
  //   }
  // }
  /**
   * For exercise 7.1 -- end
   */

  /**
   * Exercise 7.2
   * Before continuing, try to come up with representations for Par
   * that make it possible to implement the functions of our API.
   */
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // original
  // def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
  //   UnitFuture(f(a(es).get, b(es).get))
  // }

  /**
   * Exercise 7.3 - Hard
   *
   * Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.
   */
  /** Wrong ?
  def map2[A, B, C](a: Par[A], b: Par[B])
                   (f: (A, B) => C)
                   (timeout: Long, units: TimeUnit): Par[C] = (es: ExecutorService) => {
    es.submit(new Callable[C] {
      def call = {
        val start = System.nanoTime
        val av = a(es).get(timeout, units)
        val stop = System.nanoTime
        val bv = b(es).get(timeout, units)
        f(av, bv)
      }
    })
  }
   */
  /**
   * Copied from
   * https://github.com/fpinscala/fpinscala/blob/master/answerkey/parallelism/03.answer.scala
   */
  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone: Boolean = cache.isDefined
    def isCancelled: Boolean = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = {
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    }
    def get: C = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C = {
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))
    }

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        var av = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val bv = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(av, bv)
        cache = Some(ret)
        ret
    }
  }
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    Map2Future(a(es), b(es), f)
  }

  /**
   * Exercise 7.4
   *
   * This API already enables a rich set of operations.
   * Herer's a simple example: using lazyUnit,
   * write a function to convert any function A => B to one
   * that evaluates its result asynchronously.
   */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))


  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      def call = a(es).get
    })
  }
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    map2(parList, unit(()))((a, _) => a.sorted)
  }
  def map[A, B](a: Par[A])(f: A => B): Par[B] = {
    map2(a, unit(()))((a, _) => f(a))
  }
  def sortParViaMap(parList: Par[List[Int]]): Par[List[Int]] = {
    map(parList)(_.sorted)
  }
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  /**
   * Exercise 7.5 - Hard
   *
   * Write this function, called sequence.
   * No additional primitives are required. Do not call run.
   */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(lazyUnit(List()): Par[List[A]])(map2(_, _)(_::_))
  }

  /**
   * Exercise 7.6
   *
   * Implement parFilter, which filters elements of a list in parallel.
   *
   * Copied from:
   * https://github.com/fpinscala/fpinscala/blob/master/answerkey/parallelism/06.answer.scala
   */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  /**
   * Exercise 7.7 - Hard
   *
   * Given map(y)(id) == y,
   * it's a free theorem that map(map(y)(g))(f) == map(y)(f compose g).
   *
   * This is sometimes called map fusion, and it can be used as an optimization
   * - rather than spawning a separate parallel computation to compute the second mapping,
   * we can fold it into the first mapping.
   *
   * Can you prove it?
   */
  // map(y)(f.g) = f.g(y) = f(g(y)) = f(map(y)(g)) = map(map(y)(g))(f)


  /**
   * Exercise 7.8 - Hard
   *
   * Take a look through the various static methods in Executors
   * to get a feel for the different implementations of ExecutorService that exist.
   * Then, before continuing, go back and revisit your implementation of fork
   * and try to find a counterexample or convince yourself that
   * the law holds for your implementation.
   *
   * There's actually a rather subtle problem that
   * will occur in most implementations of fork.
   * When using an ExecutorService backed by a thread pool of bounded size
   * (see Executors.newFixedThreadPool),
   * it's very easy to run into a deadlock.
   *
   * val a = lazyUnit(42 + 1)
   * val S = Executors.newFixedThreadPool(1)
   * println(Par.equal(S)(a, fork(a)))
   */


  /**
   * Executors 7.9 - Hard
   *
   * Show that any fixed-size thread pool
   * can be made to deadlock given this implementation of fork.
   */
  // suppose the thread pool size is n,
  // calculate the n+1 fork function f(f(f...(f(x)))) will cause the deadlock.


  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)
  }

  /**
   * Exercise 7.11
   *
   * Implement choiceN and then choice in terms of choiceN.
   */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val index = run(es)(n).get
      val choice: Par[A] = choices.take(index + 1).last
      choice(es)
    }
  }
  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(Par.map(cond)(if (_) 0 else 1))(List(t, f))
  }

  // scalastyle:on noimpl

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
  }
}
