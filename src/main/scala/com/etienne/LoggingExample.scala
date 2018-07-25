package com.etienne

object LoggingExample {

  @MethodLogger
  def prettyPrintList(personParam: Person, str: String): Unit = {
    println("body")
  }

}