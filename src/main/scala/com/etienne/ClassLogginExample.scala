package com.etienne

@ClassLogger("logger")
class ClassLogginExample {

  import play.api.Logger
  protected val logger: Logger = Logger(this.getClass)

  def help(str: String) = {
    println(str)
  }

  def wanted() = {
    println("wanted")
  }

}