package com.etienne.logger.clazz

import org.scalatest._

import scala.language.experimental.macros

class ClassLoggerTest extends FunSuite with BeforeAndAfter {

  test("new pizza has zero toppings") {
    ClassLogginExample.help("help")(3)
    ClassLogginExample.wanted()
    ClassLogginExample.noLog()
  }

//  @ClassLogger
  object ClassLogginExample {
    //  import play.api.Logger
    //  private val hallo = Logger(this.getClass)

//    @NoLogging
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

}