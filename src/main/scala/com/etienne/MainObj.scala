package com.etienne

object MainObj extends App {
  val cle = new ClassLogginExample
  cle.help("help")(3)
  cle.wanted()
}

@ClassLoggerTest
class ClassLogginExample {

  val dog: Person = com.etienne.Person("Peter", 5)

  def help(str: String)(h: Int): Unit = {
    println(str)
  }

  def wanted(): Unit = {
    println("wanted")
  }

}