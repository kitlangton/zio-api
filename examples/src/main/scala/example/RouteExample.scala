package example

import zio._
import zio.api._

import java.util.UUID

/**   - API DSL
  *   - Compile-time ensured handler <-> api correspondence
  *   - Direct to HttpApp
  *   - optimization opportunities
  *     - dynamic dispatch
  *   - Path/Query/Header parameter API
  *   - Streaming
  */
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
