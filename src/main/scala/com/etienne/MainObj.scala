package com.etienne

object MainObj extends App {
  //  LoggingExample.prettyPrintList(Person("Peter", 6), "mrd")
  val cle = new ClassLogginExample
  cle.help("help")
  cle.wanted()
}

@ClassLogger
class ClassLogginExample {

//  import play.api.Logger
//  private val logger: Logger = Logger(this.getClass)

  def help(str: String): Unit = {
    println(str)
  }

  def wanted(): Unit = {
    println("wanted")
  }

}