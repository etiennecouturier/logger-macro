package com.etienne

import com.etienne.logger.clazz.{ClassLogger, ClassLoggerTest, NoLogging}

object MainObj extends App {
//  val cle = new ClassLogginExample
//  cle.help("help")(3)
//  cle.wanted()
//  cle.noLog()

//  ClassLogginExample.help("help")(3)
//  ClassLogginExample.wanted()
//  ClassLogginExample.noLog()

  val l = new ClassLogginExample()
}

@ClassLoggerTest
class ClassLogginExample {

//  import play.api.Logger
//  private val hallo = Logger(this.getClass)

  def help(str: String)(h: Int): Unit = {
    println(str)
  }

  def wanted(): Unit = {
    noLog(Person("hu", 3))
    println("wanted")
  }

//  @NoLogging
  def noLog(person: Person): Unit = {
    println(person)
  }

}