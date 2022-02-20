package example

import zhttp.http.HttpApp
import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio._
import zio.api._

import java.io.IOException
import java.util.UUID

object RouteExample extends ZIOAppDefault {

  // APIs

  val allUsers: API[Option[String], Unit, List[User]] =
    API
      .get("users")
      .query(string("name").?)
      .output[List[User]]

  val getUser: API[UUID, Unit, Option[User]] =
    API
      .get("users" / uuid)
      .output[Option[User]]

  val deleteUser =
    API
      .delete("users" / uuid)

  val apis =
    getUser ++ allUsers ++ deleteUser

  // Handlers

  val allUsersHandler =
    allUsers.handle {
      case Some(filter) =>
        Users.allUsers.map(_.filter(_.name.toLowerCase.contains(filter.toLowerCase)))
      case _ =>
        Users.allUsers
    }

  val getUserHandler =
    getUser.handle { uuid =>
      Users.getUser(uuid)
    }

  val deleteUserHandler =
    Handler.make(deleteUser) { case (uuid, _) =>
      Users.deleteUser(uuid)
    }

  val handlers =
    getUserHandler ++ allUsersHandler ++ deleteUserHandler

  val program =
    Server
      .start(8080, apis, handlers)
      .provideCustom(Users.live, Logger.live)

  override val run =
    program

}
