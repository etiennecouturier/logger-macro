package com.etienne

import coms.etiennes.MethodLogger

object LoggingExample {

  @MethodLogger
  def prettyPrintList(personParam: Person, str: String): Unit = {
    println("body")
  }

}