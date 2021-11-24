package example

import zio.magic._
import zio.route._
import zio.{App, ExitCode, URIO}

object RouteExample extends App {
  // Endpoints

  val allUsers =
    Endpoint
      .get("users")
      .query(string("name").?)
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
    allUsers.handle {
      case (Some(filter), _) =>
        UserService.allUsers.map(_.filter(_.name.toLowerCase.contains(filter.toLowerCase)))
      case _ =>
        UserService.allUsers
    }

  val getUserHandler =
    getUser.handle { case (uuid, _) =>
      UserService.getUser(uuid)
    }

  val deleteUserHandler =
    deleteUser.handle { case (uuid, _) =>
      UserService.deleteUser(uuid)
    }

  val handlers =
    getUserHandler ++ allUsersHandler ++ deleteUserHandler

  // sharing the endpoints between the client and server
  // getUser.invoke(UUID.randomUUID)
  // vs.
  // endpoints.invoke(getUser)(UUID.randomUUID)

  // Server
  val program =
    handlers
      .run(8888)
      .inject(UserService.live, Logger.live)

  // Client
  endpoints

  // Open API
  endpoints

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

}

case class Input[A](value: A)
