package fpinscala.exercises.ch07parallelism

import java.util.concurrent.{Executors, ExecutorService}

import fpinscala.exercises.ch07parallelism.Par._
import org.scalacheck._
import org.scalacheck.Prop.forAll

object ParTest extends Properties("Par") {
  val ES: ExecutorService = Executors.newCachedThreadPool

  property("Par.unit") = forAll {
    l: List[String] => Par.unit(l)(ES).get == l
  }

  property("Par.map2") = forAll {
    (a: Int, b: String, f: (Int, String) => String) => {
      Par.map2(Par.unit(a), Par.unit(b))(f)(ES).get == f(a, b)
    }
  }

  property("Par.lazyUnit") = forAll {
    (a: String) => {
      Par.lazyUnit(a)(ES).get == a
    }
  }

  property("Par.asyncF") = forAll {
    (a: Int, f: Int => String) => {
      Par.asyncF(f)(a)(ES).get == f(a)
    }
  }

  property("Par.fork") = forAll {
    (a: String) => {
      Par.fork(Par.unit(a))(ES).get == a
    }
  }

  property("Par.fork") = forAll {
    (a: String) => {
      Par.run(ES)(Par.unit(a)).get == a
    }
  }

  property("Par.sortPar") = forAll {
    (l: List[Int]) => {
      Par.sortPar(unit(l))(ES).get == l.sorted &&
      Par.sortPar(unit(l.reverse))(ES).get == l.sorted
    }
  }

  property("Par.map") = forAll {
    (a: Int, f: Int => String) => {
      map(unit(a))(f)(ES).get == f(a)
    }
  }
}
