package test

import com.etienne.logger.method.ControllerMethodLogger

//object MainObj extends App {
//  //  val cle = new ClassLogginExample
//  //  cle.help("help")(3)
//  //  cle.wanted()
//  //  cle.noLog()
//
//  //  ClassLogginExample.help("help")(3)
//  //  ClassLogginExample.wanted()
//  //  ClassLogginExample.noLog()
//
//  val l = new ClassLogginExample()
//  //  l.noLog(Person("", 5))
//  l.controllerMethod()
//}

//@ClassLoggerTest
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
  //  @MethodLogger
  def noLog(person: Person): Unit = {
    println(person)
  }

//  @MethodLogger
  def controllerMethod(): String = {
    authorized("test", (a, b) => a.age * b.age)
    ""
  }

  def authorized(s: String, f: (Person, Person) => Int): Int = {
    f(Person("", 2), Person("", 3))
  }

}