package zio.route.example

import zhttp.service.Server
import zio._
import zio.magic._

object RouteExample extends App {
  import zio.route._

  val allUsers =
    Endpoint
      .get("users")
      .query(QueryParams.string("name").?)
      .withResponse[List[User]]

  val allUsersHandler =
    Handler.make(allUsers) {
      case (Some(filter), _) =>
        UserService.allUsers.map(_.filter(_.name.toLowerCase.contains(filter.toLowerCase)))
      case _ =>
        UserService.allUsers
    }

  val app2 = EndpointParser.interpret(allUsersHandler)

  val program =
    Server
      .start(8888, app2)
      .inject(UserService.live, Logger.live)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

}
