package fpinscala.exercises.ch11monads

import org.scalacheck._
import org.scalacheck.Prop._

import Functor._

object MonadTest extends Properties("Monad") {

  property("identity for monads.map should hold") = forAll {
    l1: List[String] => {
      listFunctor.map(l1)(a => a) == l1
    }
  }
}
