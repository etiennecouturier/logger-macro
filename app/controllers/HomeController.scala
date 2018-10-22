package controllers

import javax.inject._
import play.api.mvc
import play.api.mvc.Controller

@Singleton
class HomeController @Inject() extends Controller {

  def index = mvc.Action {
    Ok("It works!")
  }

}