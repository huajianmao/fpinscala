package cn.hjmao.fp.scala.example

object Hello extends App {
  val p = Person("Alvin Alexander")
  println("Hello from " + p.name)
}

case class Person(var name: String)
