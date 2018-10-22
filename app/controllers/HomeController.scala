package controllers

import com.etienne.logger.clazz.ControllerLogger
import javax.inject._
import lu.foyer.play.auth.authz.{AuthorizationService, Authorized}
import play.api.mvc.{Action, AnyContent, Controller}

import scala.concurrent.Future

@Singleton
@ControllerLogger
class HomeController @Inject()(implicit auth: AuthorizationService) extends Controller {

  def index = Action { requ =>
    Ok("It works!")
  }

  def noLog = Action {
    Ok("It works!")
  }

  def withAuth: Action[AnyContent] =
    Authorized.async("urn:api:prestations-medicales:surcouche:hello", "read") { implicit request =>
      Future.successful(Ok("It works!"))
    }

}