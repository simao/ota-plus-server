package com.advancedtelematic.auth.oidc

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.api.{ApiClientExec, ApiClientSupport}
import com.advancedtelematic.auth.Tokens
import com.advancedtelematic.libats.data.DataType.Namespace
import javax.inject.Inject
import play.api.Configuration
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

trait NamespaceProvider extends (Tokens => Future[Namespace])

class NamespaceFromIdentity extends NamespaceProvider {
  override def apply(tokens: Tokens): Future[Namespace] = FastFuture.successful{
    Namespace(tokens.idToken.userId.id)
  }
}

class NamespaceFromUserProfile @Inject()(val conf: Configuration,
                                         val ws: WSClient,
                                         val clientExec: ApiClientExec)
                                        (implicit ec: ExecutionContext)
  extends NamespaceProvider with ApiClientSupport {

  override def apply(tokens: Tokens): Future[Namespace] =
    userProfileApi.userOrganizations(tokens.idToken.userId).map {
      case organizations if organizations.isEmpty =>
        Namespace.generate
      case organizations =>
        organizations.filter(_.isCreator).map(_.namespace).head
    }
}


class ConfiguredNamespace @Inject()(configuration: Configuration) extends NamespaceProvider {
  override def apply(tokens: Tokens): Future[Namespace] = FastFuture.successful {
    Namespace(configuration.get[String]("oidc.namespace"))
  }
}