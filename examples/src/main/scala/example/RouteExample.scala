package example

import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio._
import zio.api.Handler.WithId
import zio.api._
import zio.json.{uuid => _, _}

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

  val allUsers =
    API
      .get("users")
      .query(string("name").?)
      .output[List[User]]

  val getUser =
    API
      .get("users" / uuid)
      .output[Option[User]]

  val createUser =
    API
      .post("users")
      .input[CreateUser]
      .output[User]

  val deleteUser =
    API
      .delete("users" / uuid)

  val apis =
    getUser ++ allUsers ++ deleteUser ++ createUser

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

  val createUserHandler =
    createUser.handle { case CreateUser(name, email) =>
      Users.createUser(name, email)
    }

  val deleteUserHandler =
    Handler.make(deleteUser) { case (uuid, _) =>
      Users.deleteUser(uuid)
    }

  val handlers =
    getUserHandler ++ allUsersHandler ++ deleteUserHandler ++ createUserHandler

  val program =
    Server
      .start(8080, apis, handlers)
      .provideCustom(Users.live, Logger.live)

  override val run =
    program

}

final case class CreateUser(name: String, email: String)

object CreateUser {
  implicit val codec: JsonCodec[CreateUser] = DeriveJsonCodec.gen[CreateUser]
}

object ExampleClient extends ZIOAppDefault {

  // - call directly without environment
  // - HttpClient trait/Service

  // def makeClient(api: API[_,_,_]*, host: String): Client[]
  // client ++ otherClient
  //
  // test implementation of a Client that allows for

  val run =
    RouteExample.allUsers
      .call("http://localhost:8080")(Some("Langton"))
      .map(_.mkString("\n"))
      .debug
      .provideCustom(EventLoopGroup.auto(), ChannelFactory.auto)
}
