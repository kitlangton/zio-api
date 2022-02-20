package example

import zhttp.http.HttpApp
import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio._
import zio.route._

import java.io.IOException
import java.util.UUID

object RouteExample extends ZIOAppDefault {

  val allUsers: Endpoint[Option[String], Unit, List[User]] =
    Endpoint
      .get("users")
      .query(string("name").?)
      .output[List[User]]

  val getUser: Endpoint[UUID, Unit, Option[User]] =
    Endpoint
      .get("users" / uuid)
      .output[Option[User]]

  val deleteUser =
    Endpoint
      .delete("users" / uuid)

  val endpoints =
    getUser ++ allUsers ++ deleteUser

  // Handlers

  val allUsersHandler =
    allUsers.handle {
      case Some(filter) =>
        UserService.allUsers.map(_.filter(_.name.toLowerCase.contains(filter.toLowerCase)))
      case _ =>
        UserService.allUsers
    }

  val getUserHandler =
    getUser.handle { uuid =>
      UserService.getUser(uuid)
    }

  val deleteUserHandler =
    Handler.make(deleteUser) { case (uuid, _) =>
      UserService.deleteUser(uuid)
    }

  val handlers =
    getUserHandler ++ allUsersHandler ++ deleteUserHandler

  val shrimp: HttpApp[Console, IOException] =
    Endpoint
      .get("shrimp" / string)
      .toHttp { string =>
        Console.printLine(s"You asked for $string").as(string)
      }

  val program =
    Server
      .start(8080, endpoints, handlers)
      .provideCustom(UserService.live, Logger.live)

  override val run =
    program

//  lazy val request =
//    ClientParser
//      .request(allUsers)(Some("olive"))
//      .flatMap(_.bodyAsString)
//      .debug
//      .provideCustom(EventLoopGroup.auto(), ChannelFactory.auto)

}
