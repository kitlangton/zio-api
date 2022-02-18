package example

import zhttp.http.HttpApp
import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio.route._
import zio._
import zio.json.{uuid => _, _}
import zio.route.Handler.WithId

import java.io.IOException
import java.util.UUID

// HTTP Protocol
// Instead of calling methods on traits, we send HTTP Requests to the server
// And instead of merely returning a result, the server returns an HTTP Response
// A computer called a server, exposes a port and listens for requests
trait ServerTrait {
  // String -> Request
  // Request match ...
  // Response -> String

  // All http methods: GET, POST, PUT, PATCH, DELETE
  // HEADERS - (UserAgent - Safari, Accept - application/json)

  // REQUEST(method: Method, path: String, headers: Map[String, String], body: String)

  // GET /users?name=kit
  // GET /users
  def allUsers(nameFilter: Option[String]): List[User]

  // GET /users/:id
  def getUser(id: UUID): Option[User]

  // POST /users
  // body: { name: "John", age: 30 }
  def createUser(user: User): User

  // PATCH /users/:id
  // body: { name: "John", age: 30 }
  def updateUser(id: UUID, user: User): User

  // RESOURCE: Users
  // Restful
  // Get all - GET /users
  // Get one - GET /users/1234
  // Create - POST /users
  // Update - PUT /users/1234
}

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
    Handler.make(getUser) { case (uuid, _) =>
      UserService.getUser(uuid)
    }

  val deleteUserHandler =
    Handler.make(deleteUser) { case (uuid, _) =>
      UserService.deleteUser(uuid)
    }

  val handlers =
    getUserHandler ++ allUsersHandler ++ deleteUserHandler

  import Endpoint.EndpointOps
  import Endpoint.unitCodec

  val shrimp: HttpApp[Console, IOException] =
    Endpoint
      .get("shrimp" / string)
      .toHttp { string =>
        Console.printLine(s"You asked for $string").as(string)
      }

  val program =
    zhttp.service.Server
      .start(8080, shrimp)
      .provideCustom(UserService.live, Logger.live)

  override val run =
    request.delay(1.seconds) &> program

  lazy val request =
    ClientParser
      .request(allUsers)(Some("olive"))
      .flatMap(_.bodyAsString)
      .debug
      .provideCustom(EventLoopGroup.auto(), ChannelFactory.auto)

}
