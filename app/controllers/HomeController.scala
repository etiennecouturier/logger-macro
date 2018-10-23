package controllers

import javax.inject._
import lu.foyer.play.auth.authz.{AuthorizationService, Authorized}
import play.api.mvc.{Action, AnyContent, Controller}

import scala.concurrent.Future

@Singleton
//@ControllerLogger
class HomeController @Inject()(implicit auth: AuthorizationService) extends Controller {

//  @ControllerMethodLogger
  def index = Action { request =>
    Ok(request.method)
  }

  def noLog = Action {
    Ok("Nolog works!")
  }

  def withAuth: Action[AnyContent] =
    Authorized.async("urn:api:prestations-medicales:surcouche:hello", "read") { implicit request =>
      Future.successful(Ok("It works!"))
    }

}