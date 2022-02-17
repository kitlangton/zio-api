package example

import zio.magic._
import zio.route._
import zio._

object RouteExample extends App {
// Endpoints
//  Endpoint
//    .get("users" / "posts" / Path.int / Path[UUID])
//    .query(Query.string("name").?)

  val allUsers =
    Endpoint
      .get("users")
      .query(string("name").?)
      .header(Headers.Accept)
      .withResponse[List[User]]

  val getUser =
    Endpoint
      .get("users" / uuid)
      .withResponse[Option[User]]

  val deleteUser =
    Endpoint
      .delete("users" / uuid)

  val endpoints =
    getUser ++ allUsers ++ deleteUser

  // Handlers

  val allUsersHandler =
    Handler.make(allUsers) {
      case ((Some(filter), _), _) =>
        UserService.allUsers.map(_.filter(_.name.toLowerCase.contains(filter.toLowerCase)))
      case _ =>
        UserService.allUsers
    }

  val getUserHandler =
    Handler.make(getUser) { case (uuid, _) =>
      UserService.getUser(uuid)
    }

  val deleteUserHandler =
    Handler.make(deleteUser) { case (uuid, _) =>
      UserService.deleteUser(uuid)
    }

  val handlers =
    getUserHandler ++ allUsersHandler ++ deleteUserHandler

  val program =
    Server(endpoints, handlers)
      .run(8888)
      .provide(UserService.live, Logger.live)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    UIO(EndpointParser.parseUrl(allUsers.requestParser)((Some("kit"), "json"))).debug.exitCode

}
