package com.advancedtelematic.controllers

import java.time.Instant
import akka.Done
import akka.actor.ActorSystem
import com.advancedtelematic.PlayMessageBusPublisher
import com.advancedtelematic.api.Errors.{OtaUserDoesNotExists, OtaUserIsDeactivated, UnexpectedResponse}
import com.advancedtelematic.auth._
import com.advancedtelematic.auth.oidc.{NamespaceProvider, OidcGateway}
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libats.messaging_datatype.MessageLike

import javax.inject.{Inject, Singleton}
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

final case class LoginData(username: String, password: String)

final case class UserLogin(id: String, identity: Option[IdentityClaims], namespace: Namespace, timestamp: Instant)

case class RejectedNewUserLogin(userId: UserId, name: String, email: String, when: Instant = Instant.now)

object UserLogin {
  private[this] val log = Logger(this.getClass)

  import com.advancedtelematic.libats.codecs.CirceAnyVal.{anyValStringDecoder, anyValStringEncoder}

  private[this] implicit val identityClaimsCodec = io.circe.generic.semiauto.deriveCodec[IdentityClaims]
  private[this] implicit val userLoginCodec = io.circe.generic.semiauto.deriveCodec[UserLogin]
  private[this] implicit val rejectedNewUserLoginCodec = io.circe.generic.semiauto.deriveCodec[RejectedNewUserLogin]

  implicit val userLoginMsgLike = MessageLike[UserLogin](_.id)
  implicit val rejectedNewUserLoginMsgLike = MessageLike[RejectedNewUserLogin](_.userId.id)

  def apply(futureIdentityClaims: Future[IdentityClaims], userId: String, namespace: Namespace)
           (implicit ec: ExecutionContext): Future[UserLogin] =
    futureIdentityClaims
      .map(claim => UserLogin(claim.userId.id, Some(claim), namespace, Instant.now()))
      .recover {
        case t =>
          log.warn("Unable to get user info", t)
          UserLogin(userId, None, namespace, Instant.now())
      }
}

final case class UnexpectedToken(token: IdToken, msg: String) extends Throwable {
  override def getMessage: String =
    s"Cannot parse namespace from token: '${token.value}', '${msg}'"
}

final case class CallbackError(msg: String) extends Throwable(msg)

class LoginController @Inject()(components: ControllerComponents,
                                val login: LoginAction,
                                val logout: LogoutAction)
    extends AbstractController(components) {

  val accountConfirmation: Action[AnyContent] = Action {
    Ok(views.html.accountConfirmation())
  }

  val accountActivated: Action[AnyContent] = Action {
    Ok(views.html.activated())
  }
}

/**
  * Handles just login/authentication
  */
@Singleton
class OAuthOidcController @Inject()(
    oidcGateway: OidcGateway,
    messageBus: PlayMessageBusPublisher,
    tokenExchange: TokenExchange,
    namespaceProvider: NamespaceProvider,
    components: ControllerComponents
)(implicit system: ActorSystem)
    extends AbstractController(components) {

  implicit val ec = components.executionContext

  private[this] val log = Logger(this.getClass)

  lazy val config = system.settings.config

  private[this] val RedirectToLogin = Redirect(com.advancedtelematic.controllers.routes.LoginController.login())

  val authorizationError: Action[AnyContent] = Action { implicit request =>
    Unauthorized(views.html.authorizationError())
  }

  def authorizationFailed(error: String, errorDescription: String): Result = {
    Redirect(routes.OAuthOidcController.authorizationError()).flashing("authzError" -> error)
  }

  def publishLoginEvent(userId: UserId, namespace: Namespace, accessToken: AccessToken): Future[Done] =
    UserLogin(oidcGateway.getUserInfo(accessToken), userId.id, namespace)
      .flatMap(messageBus.publish(_)).map(_ => Done)

  val callback: Action[AnyContent] = Action.async { request =>
    def onAuthzError(errorCode: String): Future[Result] = {
      val result = request
        .getQueryString("error_description")
        .map(authorizationFailed(errorCode, _))
        .getOrElse(Redirect(routes.LoginController.login()))
      Future.successful(result)
    }

    def onAuthzCode(code: String): Future[Result] = {
      val loginResult = for {
        tokens                                   <- oidcGateway.exchangeCodeForTokens(code)
        newTokens @ Tokens(accessToken, idToken) <- tokenExchange.run(tokens)
        ns <- namespaceProvider(newTokens)
        _ <- publishLoginEvent(idToken.userId, ns, accessToken)
      } yield
        Redirect(com.advancedtelematic.controllers.routes.Application.index()).withSession(
          "namespace"    -> ns.get,
          "id_token"     -> idToken.value,
          "access_token" -> Json.stringify(Json.toJson(accessToken)(SessionCodecs.AccessTokenFormat))
        )
      loginResult.recover {
        case t: UnexpectedResponse =>
          log.debug("Error while exchanging authz code for tokens", t)
          RedirectToLogin
        case e@OtaUserDoesNotExists =>
          authorizationFailed(e.code.code, e.desc)
        case e@OtaUserIsDeactivated =>
          authorizationFailed(e.code.code, e.desc)
      }
    }

    request.getQueryString("error") match {
      case Some(errorCode) =>
        onAuthzError(errorCode)
      case None =>
        request.getQueryString("code").fold(Future.successful(RedirectToLogin))(onAuthzCode)
    }
  }

}
