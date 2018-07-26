package com.etienne

object MainObj extends App {
  val cle = new ClassLogginExample
  cle.help("help")(3)
  cle.wanted()
}

@ClassLogger
class ClassLogginExample {

  /*import play.api.Logger
  private val logger = Logger(this.getClass)*/

  def help(str: String)(h: Int): Unit = {
    println(str)
  }

  def wanted(): Unit = {
    println("wanted")
  }

}