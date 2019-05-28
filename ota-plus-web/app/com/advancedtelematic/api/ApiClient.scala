package com.advancedtelematic.api

import java.util.UUID

import akka.Done
import akka.stream.Materializer
import akka.util.ByteString
import com.advancedtelematic.api.ApiRequest.UserOptions
import com.advancedtelematic.auth.AccessToken
import com.advancedtelematic.controllers.{FeatureName, UserId}
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libtuf.data.TufCodecs.tufKeyPairDecoder
import com.advancedtelematic.libtuf.data.TufDataType.TufKeyPair
import play.api.Configuration
import play.api.http.Status
import play.api.libs.json._
import play.api.libs.ws.{BodyWritable, InMemoryBody, WSAuthScheme, WSClient, WSRequest, WSResponse}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NoStackTrace

object ApiVersion extends Enumeration {
  type ApiVersion = Value
  val v1, v2 = Value
}

case class RemoteApiIOError(cause: Throwable) extends Exception(cause) with NoStackTrace

case class RemoteApiError(result: Result, msg: String = "") extends Exception(msg) with NoStackTrace

case class RemoteApiParseError(msg: String) extends Exception(msg) with NoStackTrace

case class UserPass(user: String, pass: String)

case class Feature(feature: FeatureName, client_id: Option[UUID], enabled: Boolean)

case class UserOrganization(namespace: Namespace, name: String, isCreator: Boolean)

object ApiRequest {
  case class UserOptions(token: Option[String] = None,
                         authuser: Option[UserPass] = None,
                         namespace: Option[Namespace] = None)

  def base(baserUrl: String): String => ApiRequest = apply(baserUrl)

  def apply(baseUrl: String)(path: String): ApiRequest = new ApiRequest {
    override def build = ws => ws.url(baseUrl + path).withFollowRedirects(false)
  }
}

trait CirceJsonBodyWritables {

  implicit def circeJsonBodyWritable: BodyWritable[io.circe.Json] =
    BodyWritable(json => InMemoryBody(ByteString.fromString(json.noSpaces)), "application/json")

}

/**
  * Common utilities to simplify building requests for micro-services.
  * <p>
  * Each [[OtaPlusConfig]] subclass ( one each for core, resolver, device-registry, and auth-plus )
  * owns a dedicated [[ApiRequest]] created via [[ApiRequest.apply]].
  */
trait ApiRequest { self =>
  def build: WSClient => WSRequest

  def withUserOptions(userOptions: UserOptions): ApiRequest = {
    self
      .withAuth(userOptions.authuser)
      .withToken(userOptions.token)
      .withNamespace(userOptions.namespace)
  }

  def withAuth(auth: Option[UserPass]): ApiRequest = {
    auth map { u =>
      transform(_.withAuth(u.user, u.pass, WSAuthScheme.BASIC))
    } getOrElse self
  }

  def withNamespace(ns: Option[Namespace]): ApiRequest = {
    ns map { n =>
      transform(_.addHttpHeaders("x-ats-namespace" -> n.get))
    } getOrElse self
  }

  def withToken(token: String): ApiRequest =
    transform(_.addHttpHeaders(("Authorization", "Bearer " + token)))

  def withToken(token: Option[String]): ApiRequest =
    token.map(withToken).getOrElse(self)

  def transform(f: WSRequest => WSRequest): ApiRequest = new ApiRequest {
    override def build = ws => f(self.build(ws))
  }

  def execResponse(apiExec: ApiClientExec): Future[WSResponse] = {
    apiExec.runUnsafe(build)
  }

  def execResult(apiExec: ApiClientExec): Future[Result] = {
    apiExec.runApiResult(build)
  }

  def execStreamedResult(apiExec: ApiClientExec): Future[Result] = {
    apiExec.runStreamedResult(build)
  }

  def execJsonValue(apiExec: ApiClientExec): Future[JsValue] = {
    apiExec.runApiJsonValue(build)
  }

  def execJson[T](apiExec: ApiClientExec)(implicit ev: Reads[T]): Future[T] = {
    apiExec.runApiJson[T](build)
  }
}

/**
  * Controllers extending [[ApiClientSupport]] access [[AuthPlusApi]] endpoints using a singleton.
  */
class AuthPlusApi(val conf: Configuration, val apiExec: ApiClientExec) extends OtaPlusConfig {

  private val authPlusRequest = ApiRequest.base(authPlusApiUri + "/")

  /**
    * The response is json for `com.advancedtelematic.authplus.client.ClientInformationResponse`
    * for a ClientID:
    * <ul>
    *   <li>the ClientID was generated by Auth+ upon registering a DeviceID</li>
    *   <li>the web-app persisted the association DeviceID -> ClientID</li>
    * </ul>
    */
  def fetchClientInfo(clientID: UUID, token: AccessToken): Future[JsValue] = {
    authPlusRequest(s"clients/${clientID.toString}")
      .withToken(token.value)
      .transform(_.withMethod("GET"))
      .execJsonValue(apiExec)
  }

  def fetchSecret(clientID: UUID, token: AccessToken)(implicit ec: ExecutionContext): Future[String] = {
    fetchClientInfo(clientID, token).flatMap { parsed =>
      val t2: Try[String] = Try((parsed \ "client_secret").validate[String].get)
      Future.fromTry(t2)
    }
  }

}

class UserProfileApi(val conf: Configuration, val apiExec: ApiClientExec) extends OtaPlusConfig {

  import play.api.libs.functional.syntax._

  private val userProfileRequest = ApiRequest.base(userProfileApiUri + "/api/v1/")

  implicit val featureNameR: Reads[FeatureName] = Reads.StringReads.map(FeatureName)
  implicit val featureNameW: Writes[FeatureName] = Writes.StringWrites.contramap(_.get)
  implicit val namespaceR: Reads[Namespace] = Reads.StringReads.map(Namespace(_))

  implicit val featureR: Reads[Feature] = {(
    (__ \ "feature").read[FeatureName] and
    (__ \ "client_id").readNullable[UUID] and
    (__ \ "enabled").read[Boolean]
  )(Feature.apply _)}

  implicit val userOrganizationR: Reads[UserOrganization] = {
    (
      (__ \ "namespace").read[Namespace] and
      (__ \ "name").read[String] and
      (__ \ "isCreator").read[Boolean]
    )(UserOrganization.apply _)
  }

  def getUser(userId: UserId): Future[JsValue] =
    userProfileRequest("users/" + userId.id).execJsonValue(apiExec)

  def getFeature(namespace: Namespace, feature: FeatureName): Future[Feature] =
    userProfileRequest("organizations/" + namespace.get + "/features/" + feature.get).execJson[Feature](apiExec)

  def getFeatures(namespace: Namespace): Future[Seq[FeatureName]] =
    userProfileRequest("organizations/" + namespace.get + "/features").execJson[Seq[FeatureName]](apiExec)

  def activateFeature(namespace: Namespace, feature: FeatureName, clientId: UUID): Future[Result] = {
    val requestBody = Json.obj("feature" -> feature.get, "client_id" -> clientId)

    userProfileRequest(s"organizations/${namespace.get}/features")
      .transform(_.withMethod("POST").withBody(requestBody))
      .execResult(apiExec)
  }

  def updateDisplayName(userId: UserId, newName: String)(implicit executionContext: ExecutionContext): Future[Done] = {
    val request = userProfileRequest(s"users/${userId.id}/displayname").transform(
      _.withMethod("PUT").withBody(JsString(newName))
    ).build
    apiExec.runSafe(request).map(_ => Done)
  }

  def updateBillingInfo[T](userId: UserId, query: Map[String,Seq[String]], body: JsValue): Future[Result] =
    userProfileRequest(s"users/${userId.id}/billing_info")
      .transform(
        _.withMethod("PUT")
          .withQueryStringParameters(query.mapValues(_.head).toSeq :_*)
          .withBody(body))
      .execResult(apiExec)

  def userProfileRequest(userId: UserId, method: String, path: String): Future[Result] =
    userProfileRequest(s"users/${userId.id}/${path}").transform(_.withMethod(method))
      .execResult(apiExec)

  def userOrganizations(userId: UserId): Future[Set[UserOrganization]] =
    userProfileRequest(s"users/${userId.id}/organizations")
      .transform(_.withMethod("GET"))
      .execJson[Set[UserOrganization]](apiExec)

  def namespaceIsAllowed(userId: UserId, namespace: Namespace)(implicit ec: ExecutionContext): Future[Boolean] =
    userOrganizations(userId).map(_.map(_.namespace)).map(_.contains(namespace))

  def getNamespaceSetupStatus(namespace: Namespace): Future[Result] =
    userProfileRequest(s"organizations/${namespace.get}/setup")
      .transform(_.withMethod("GET"))
      .execResult(apiExec)
}

class RepoServerApi(val conf: Configuration, val apiExec: ApiClientExec) extends OtaPlusConfig
                                                                                 with CirceJsonBodyWritables {
  private val request = ApiRequest.base(repoApiUri + "/api/v1/")

  def rootJsonResult(namespace: Namespace): Future[Result] =
    request("user_repo/root.json")
      .withNamespace(Some(namespace))
      .execResult(apiExec)
}

class KeyServerApi(val conf: Configuration, val apiExec: ApiClientExec) extends OtaPlusConfig {
  private val request = ApiRequest.base(keyServerApiUri + "/api/v1/")

  def targetKeys(repoId: String)(implicit ec: ExecutionContext, mat: Materializer): Future[Seq[TufKeyPair]] = {
    // using circe since there is a decoder for it in the lib
    import io.circe.{parser => CirceParser}

    for {
      result <- request(s"root/$repoId/keys/targets/pairs").execResult(apiExec)
      byteString <- result.body.consumeData
    } yield {
      if (result.header.status == Status.NOT_FOUND) {
        Seq.empty
      } else {
        val parsed = CirceParser.parse(byteString.utf8String) match {
          case Left(t) =>
            throw RemoteApiParseError(s"error parsing target keys (${byteString.utf8String}): ${t.message}")
          case Right(json) =>
            json
        }

        val vector = parsed.asArray.getOrElse(throw new Exception("Vector expected"))
        vector.map { json =>
          tufKeyPairDecoder.decodeJson(json) match {
            case Left(t) => throw t
            case Right(keyPair) => keyPair
          }
        }
      }
    }
  }

}
