package com.advancedtelematic.controllers

import cats.syntax.show._
import com.advancedtelematic.AuthenticatedAction
import com.advancedtelematic.api.{ApiClientExec, ApiClientSupport, RemoteApiError}
import javax.inject.{Inject, Singleton}

import org.genivi.sota.data.Uuid
import play.api.Configuration
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc.Results.EmptyContent
import play.api.mvc.{AbstractController, Action, AnyContent, BodyParsers, Controller, ControllerComponents}

import scala.concurrent.{ExecutionContext, Future}


final case class UserProfile(fullName: String, email: String, picture: String, profile: Option[JsValue])

final case class UserId(id: String) extends AnyVal

object UserProfile {

  val FromUserInfoReads: Reads[UserProfile] =
    (((__ \ "user_metadata" \ "name").read[String] | (__ \ "name").read[String]) and
      (__ \ "email").read[String] and
      (__ \ "picture").read[String] and
      ((__ \ "user_metadata" \ "profile").readNullable[JsValue] |
        Reads.pure(Option.empty[JsValue])))(UserProfile.apply _)

  implicit val FormatInstance: Format[UserProfile] = Json.format[UserProfile]
}

@Singleton
class UserProfileController @Inject()(val conf: Configuration, val ws: WSClient, val clientExec: ApiClientExec,
                                      authAction: AuthenticatedAction, components: ControllerComponents)(
    implicit exec: ExecutionContext)
    extends AbstractController(components)
    with ApiClientSupport {

  import com.advancedtelematic.JsResultSyntax._
  import com.advancedtelematic.controllers.FeatureName

  def getUserProfile: Action[AnyContent] = authAction.async { request =>
    val fut = for {
      user <- auth0Api.queryUserProfile(request.auth0AccessToken)
      profile <- userProfileApi.getUser(request.idToken.userId)
    } yield Ok(Json.toJson(user.copy(profile = Some(profile))))

    fut.recover {
      case e: RemoteApiError => e.result.header.status match {
        case Unauthorized.header.status => Forbidden.sendEntity(e.result.body)
        case _ => e.result
      }
    }
  }

  val changePassword: Action[AnyContent] = authAction.async { request =>
    for {
      _     <- auth0Api.changePassword(request.idToken.email)
    } yield Ok(EmptyContent())
  }

  def updateUserProfile(): Action[JsValue] = authAction.async(components.parsers.json) { request =>
    import com.advancedtelematic.JsResultSyntax._
    (request.body \ "name").validate[String].toEither match {
      case Right(newName) =>
        auth0Api.saveUserMetadata(request.idToken, "name", JsString(newName)).map { userInfo =>
          Ok(Json.toJson(userInfo))
        }

      case Left(err) =>
        Future.successful(BadRequest(err))
    }
  }

  def updateBillingInfo(): Action[JsValue] = authAction.async(components.parsers.json) { request =>
    userProfileApi.updateBillingInfo(request.idToken.userId, request.queryString, request.body)
  }

  implicit val featureW: Writes[FeatureName] = Writes.StringWrites.contramap(_.get)

  def getFeatures(): Action[AnyContent] = authAction.async { request =>
    val userId = request.idToken.userId
    userProfileApi.getFeatures(userId)
      .map { features => Ok(Json.toJson(features)) }
      .recover {
        case RemoteApiError(r, _) if (r.header.status == 404) =>
          Ok(Json.toJson(Seq.empty[FeatureName]))
        case RemoteApiError(r, _) => r
      }
  }

  val apiDomain = conf.get[String]("api.domain")

  def activateFeature(feature: FeatureName): Action[AnyContent] = authAction.async { request =>
    val userId = request.idToken.userId
    val token = request.authPlusAccessToken

    def activateFeatureWithClientId = for {
      clientInfo <- authPlusApi.createClientForUser(
        feature.get, s"namespace.${request.namespace.get} $apiDomain/${feature.get}", token)
      clientId = Uuid.fromJava(clientInfo.clientId)
      result <- userProfileApi.activateFeature(userId, feature, clientId)
    } yield result

    userProfileApi.getFeature(userId, feature)
      .map { r => r.client_id }
      .recover { case RemoteApiError(r, _) if (r.header.status == 404) => None }
      .flatMap { client_id => client_id match {
        case Some(id) => Future.successful(Ok(EmptyContent()))
        case None => activateFeatureWithClientId
      }
    }
  }

  def getFeatureClient(feature: FeatureName): Action[AnyContent] = authAction.async { request =>
    val userId = request.idToken.userId
    val token = request.authPlusAccessToken

    userProfileApi.getFeature(userId, feature).flatMap { f =>
      f.client_id match {
        case Some(id) => authPlusApi.getClient(id, token)
        case None => Future.successful(NotFound)
      }
    }
  }

  def getFeatureConfig(feature: FeatureName): Action[AnyContent] = authAction.async { request =>
    val userId = request.idToken.userId
    val token = request.authPlusAccessToken

    userProfileApi.getFeature(userId, feature).flatMap { f =>
      f.client_id match {
        case Some(id) => for {
          secret <- authPlusApi.fetchSecret(id.toJava, token)
          result <- buildSrvApi.download(feature.get, id, secret)
        } yield result
        case None => Future.successful(NotFound)
      }
    }
  }

}
