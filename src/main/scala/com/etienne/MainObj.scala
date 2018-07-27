package com.etienne

import coms.etiennes.{ClassLogger, NoLogging}

object MainObj extends App {
  val cle = new ClassLogginExample
  cle.help("help")(3)
  cle.wanted()
  cle.noLog()
}

@ClassLogger
class ClassLogginExample {

//  import play.api.Logger
//  private val hallo = Logger(this.getClass)

  def help(str: String)(h: Int): Unit = {
    println(str)
  }

  def wanted(): Unit = {
    println("wanted")
  }

  @NoLogging
  def noLog(): Unit = {
    println("noLog")
  }

}