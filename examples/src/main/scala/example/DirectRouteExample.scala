package example

import zhttp.http.{Http, HttpApp}
import zio._
import zio.api._

import java.util.UUID

object DirectRouteExample extends ZIOAppDefault {

  // APIs

  val allUsers: HttpApp[Users, Throwable] =
    API
      .get("users")
      .query(string("name").?)
      .output[List[User]]
      .toHttp {
        case Some(filter) =>
          Users.allUsers.map(_.filter(_.name.toLowerCase.contains(filter.toLowerCase)))
        case _ =>
          Users.allUsers
      }

  val getUser: HttpApp[Users, Throwable] =
    API
      .get("users" / uuid)
      .output[Option[User]]
      .toHttp { uuid =>
        Users.getUser(uuid)
      }

  val deleteUser =
    API
      .delete("users" / uuid)
      .toHttp { id =>
        Users.deleteUser(id)
      }

  val app = getUser ++ allUsers ++ deleteUser

  val program =
    zhttp.service.Server
      .start(8081, app ++ Http.notFound)
      .provideCustom(Users.live, Logger.live)

  override val run = program

}
