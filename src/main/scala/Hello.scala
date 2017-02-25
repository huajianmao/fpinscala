
object Hello extends App {
  val p = Person("Alvin Alexander")
  // scalastyle:off println
  println("Hello from " + p.name)
  // scalastyle:on println
}

case class Person(var name: String)
