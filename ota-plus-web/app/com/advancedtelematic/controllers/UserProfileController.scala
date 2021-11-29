package com.advancedtelematic.controllers

import java.util.UUID
import brave.play.ZipkinTraceServiceLike
import brave.play.implicits.ZipkinTraceImplicits
import com.advancedtelematic.api.Errors.{RemoteApiError, UnexpectedResponse}
import com.advancedtelematic.api.{ApiClientExec, ApiClientSupport}
import com.advancedtelematic.auth.oidc.OidcGateway
import com.advancedtelematic.auth.{IdentityAction, IdentityClaims}

import javax.inject.{Inject, Singleton}
import play.api.{Configuration, Logger}
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

final case class UserId(id: String) extends AnyVal

@Singleton
class UserProfileController @Inject()(val conf: Configuration,
                                      val ws: WSClient,
                                      val clientExec: ApiClientExec,
                                      implicit val tracer: ZipkinTraceServiceLike,
                                      authAction: IdentityAction,
                                      oidcGateway: OidcGateway,
                                      components: ControllerComponents)(implicit exec: ExecutionContext)
  extends AbstractController(components)
    with ApiClientSupport with ZipkinTraceImplicits {

  private val log = Logger(this.getClass)

  private def parseDisplayName(claims: IdentityClaims, profile: JsValue) =
    ((profile \ "displayName").validateOpt[String] match {
      case JsSuccess(maybeName, _) => maybeName
      case JsError(_) => None
    }).getOrElse(claims.name)

  def getUserProfile: Action[AnyContent] = authAction { implicit request =>
    Ok(Json.obj(
      "fullName" -> JsString("lol"),
      "email"    -> JsString("lol@lol.pt"),
      "picture"  -> JsNull,
      "profile"  -> JsObject(Seq(
        "defaultNamespace" -> JsString("default"),
        "initialNamespace" -> JsString("default"),
        "userId" -> JsString("default"),
      ))
    ))
  }

  def getUserProfile_2: Action[AnyContent] = authAction.async { implicit request =>
    val fut = for {
      claims  <- oidcGateway.getUserInfo(request.accessToken)
      profile <- userProfileApi.getUser(claims.userId)
      displayName = parseDisplayName(claims, profile)
    } yield
      Ok(
        Json.obj(
          "fullName" -> displayName,
          "email"    -> claims.email,
          "picture"  -> claims.picture,
          "profile"  -> profile
        )
      )

    fut.recover {
      case UnexpectedResponse(u) if u.status == TOO_MANY_REQUESTS =>
        log.error(s"Too many requests: ${u.body}")
        TooManyRequests
      case e: RemoteApiError =>
        e.result.header.status match {
          case Unauthorized.header.status => Forbidden.sendEntity(e.result.body)
          case _                          => e.result
        }
    }
  }

  val changePassword: Action[AnyContent] = authAction.async { _ =>
    Future.successful {
      NotImplemented("Change password functionality is not implemented.")
    }
  }

  def updateUserProfile(): Action[JsValue] = authAction.async(components.parsers.json) { implicit request =>
    import com.advancedtelematic.JsResultSyntax._
    (request.body \ "name").validate[String].toEither match {
      case Right(newName) =>
        userProfileApi.updateDisplayName(request.idToken.userId, newName).flatMap { _ =>
          oidcGateway.getUserInfo(request.accessToken).map { claims =>
            Ok(
              Json.obj(
                "fullName" -> newName,
                "email"    -> claims.email,
                "picture"  -> claims.picture
              )
            )
          }
        }

      case Left(err) =>
        Future.successful(BadRequest(err))
    }
  }

  def updateBillingInfo(): Action[JsValue] = authAction.async(components.parsers.json) { implicit request =>
    userProfileApi.updateBillingInfo(request.idToken.userId, request.queryString, request.body)
  }

  def getUserCredentialsBundle(keyUuid: UUID): Action[AnyContent] = authAction.async { implicit request =>
    userProfileApi.getCredentialsBundle(request.namespace, keyUuid)
  }

  def proxyRequest(path: String): Action[AnyContent] = authAction.async { implicit request =>
    userProfileApi.userProfileRequest(request.idToken.userId, request.method, path, request.body.asJson)
  }

  def orgs(): Action[AnyContent] = authAction { implicit  req =>
    Results.Ok(Json.arr(Json.obj("namespace" -> JsString("default"), "name" -> JsString("default"), "memberCount" -> JsNumber(2), "createdAt" -> JsString("1970-01-01T00:00:00Z"))))
  }

  def defaultOrg(): Action[AnyContent] = authAction { implicit  req =>
    val j = """{"namespace":"default","name":"simao.mata's Organization","createdBy":"HERE-2573dcda-6ff2-4b85-8041-f5609e799ff5","createdAt":"1970-01-01T00:00:00Z"}"""
      Results.Ok(Json.parse(j))
  }

  def contracts() = authAction { implicit  req =>
    val j = """[{"user":"default","contract":"tc/2018-04/en/terms-and-conditions.html","accepted":"2018-12-13T09:33:59Z"}]"""
    Results.Ok(Json.parse(j))
  }
}
