package example

import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio._
import zio.api._
import zio.api.openapi.OpenApiInterpreter
import zio.json.{uuid => _, _}
import zio.schema.{DeriveSchema, Schema}

object ZIOWorld extends ZIOAppDefault {

  // APIs

  val allUsers =
    API
      .get("talks")
      .query(string("title").?)
      .output[List[Talk]]

  val getUser =
    API
      .get("talks" / uuid)
      .output[Option[Talk]]

  val createUser =
    API
      .post("talks")
      .input[CreateTalk]
      .output[Talk]

  val deleteUser =
    API
      .delete("talks" / uuid)

  val apis =
    getUser ++ allUsers ++ deleteUser ++ createUser

  // HANDLERS

  val allUsersHandler =
    allUsers.handle {
      case Some(filter) =>
        Talks.all.map(_.filter(_.title.toLowerCase.contains(filter.toLowerCase)))
      case _ =>
        Talks.all
    }

  val getUserHandler =
    getUser.handle { uuid =>
      Talks.get(uuid)
    }

  val createUserHandler =
    createUser.handle { case CreateTalk(title, description, duration) =>
      Talks.create(title, description, duration)
    }

  val deleteUserHandler =
    Handler.make(deleteUser) { case (uuid, _) =>
      Talks.delete(uuid)
    }

  val handlers =
    getUserHandler ++ allUsersHandler ++ deleteUserHandler ++ createUserHandler

  val program =
    Server
      .start(8080, apis, handlers)
      .provide(Talks.live, Logger.live)

  val printSchema =
    ZIO.debug(OpenApiInterpreter.generate(apis)("ZIO World", "API for ZIO World 2022 Talks"))

  override val run =
    printSchema *> program

}

final case class CreateTalk(title: String, description: String, duration: Int)

object CreateTalk {
  implicit val codec: JsonCodec[CreateTalk] = DeriveJsonCodec.gen[CreateTalk]
  implicit val schema: Schema[CreateTalk]   = DeriveSchema.gen[CreateTalk]
}

object ExampleClient extends ZIOAppDefault {

  val run =
    ZIOWorld.allUsers
      .call("http://localhost:8080")(None)
      .map(_.mkString("\n"))
      .debug
      .provide(EventLoopGroup.auto(), ChannelFactory.auto)

}
