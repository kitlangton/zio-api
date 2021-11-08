package example

import zhttp.service.Server
import zio.{App, ExitCode, Has, URIO, ZIO}
import zio.route._
import zio.magic._
import zio.route.Handler.WithId
import zio.route.macros.{Macros, Matches}

import java.util.UUID

object RouteExample extends App {
  import Route._

  // Endpoints

  val allUsers =
    Endpoint
      .get("users")
      .query(QueryParams.string("name").?)
      .withResponse[List[User]]

  val getUser =
    Endpoint
      .get("users" / uuid)
      .withResponse[Option[User]]

  val deleteUser =
    Endpoint
      .delete("users" / uuid)

  val endpoints =
    allUsers ++ getUser ++ deleteUser

  // Handlers

  val allUsersHandler =
    Handler.make(allUsers) {
      case (Some(filter), _) =>
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
    allUsersHandler ++ getUserHandler ++ deleteUserHandler

  def unite[EndpointIds, HandlerIds](endpoints: Endpoints[EndpointIds], handlers: Handlers[_, _, HandlerIds])(implicit
      matches: Matches[EndpointIds, HandlerIds]
  ) =
    "Cool"

  unite(endpoints, handlers)

  val app = EndpointParser.interpret(allUsersHandler)

  val program =
    ZIO.debug("hi")
//    Server
//      .start(8888, app)
//      .inject(UserService.live, Logger.live)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

}
