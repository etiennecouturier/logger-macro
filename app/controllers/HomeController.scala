package controllers

import com.etienne.logger.method.ControllerMethodLogger
import javax.inject._
import lu.foyer.play.auth.authz.{AuthorizationService, Authorized}
import play.api.mvc.{Action, AnyContent, Controller}

import scala.concurrent.Future

@Singleton
class HomeController @Inject() (implicit auth: AuthorizationService) extends Controller {

  @ControllerMethodLogger
  def index = Action { requ =>
    Ok("It works!")
  }

  @ControllerMethodLogger
  def noLog = Action {
    Ok("It works!")
  }

  @ControllerMethodLogger
  def withAuth: Action[AnyContent] =
    Authorized.async("urn:api:prestations-medicales:surcouche:hello", "read") { implicit request =>
      Future.successful(Ok("It works!"))
    }

}