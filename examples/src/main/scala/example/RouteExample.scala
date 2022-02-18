package example

import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio.route._
import zio._

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
  // Endpoints
  //  Endpoint
  //    .get("users" / "posts" / Path.int / Path[UUID])
  //    .query(Query.string("name").?)

  // servers and clients
  // users?name=string
  // request match {
  //   case GET -> "users" / id =>
  //     Users.getUser(id).map { user => Response.json(user.toJson) }
  // }

  // GET /users?name=kit&age=30
  // GET /users
  //  def allUsers(nameFilter: Option[String]): List[User]
  val allUsers: Endpoint[Option[String], Unit, List[User]] =
    Endpoint
      .get("users")
      .query(string("name").?)
      .withOutput[List[User]]

  val getUser: Endpoint[UUID, Unit, Option[User]] =
    Endpoint
      .get("users" / uuid)
      .withOutput[Option[User]]

  val deleteUser =
    Endpoint
      .delete("users" / uuid)

  val endpoints =
    getUser ++ allUsers ++ deleteUser

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
    getUserHandler ++ allUsersHandler ++ deleteUserHandler

  val program =
    Server(endpoints, handlers)
      .run(8080)
      .provide(UserService.live, Logger.live)

  override val run =
//    program
    ClientParser
      .request(allUsers)(Some("olive"))
      .flatMap(_.bodyAsString)
      .debug
      .provideCustom(EventLoopGroup.auto(), ChannelFactory.auto)
  //    ZIO.debug {
//      // /users?name=kit Map(Accept -> json)
//      // /users Map(Accept -> json)
//      ClientParser.parseUrl(allUsers.requestParser)((10, None))
//    }

}
