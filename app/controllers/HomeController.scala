package controllers

import com.etienne.logger.method.MethodLogger
import javax.inject._
import play.api.mvc.{Action, Controller}

@Singleton
class HomeController @Inject() extends Controller {

  @MethodLogger
  def index = Action { requ =>
    Ok("It works!")
  }

  @MethodLogger
  def noLog = Action {
    Ok("It works!")
  }

}