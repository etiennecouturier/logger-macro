package com.etienne

object MainObj extends App {
  //  LoggingExample.prettyPrintList(Person("Peter", 6), "mrd")
  val cle = new ClassLogginExample
  cle.help("help")(3)
  cle.wanted()
}

@ClassLogger
class ClassLogginExample {

  def help(str: String)(h: Int): Unit = {
    println(str)
  }

  def wanted(): Unit = {
    println("wanted")
  }

}