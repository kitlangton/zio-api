package zio.api

import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio._
import zio.json.{uuid => _, _}
import zio.test._
import zio.schema._

import java.util.UUID

object ServerClientSpec extends DefaultRunnableSpec {

  // APIs

  val usersAPI =
    API
      .get("users")
      .output[List[User]]

  val userAPI =
    API
      .get("users" / uuid)
      .output[Option[User]]

  val countAPI =
    API
      .get("counter")
      .output[Int]

  val incrementAPI =
    API
      .post("counter")
      .input[Int]

  // Handlers

  val usersHandler =
    usersAPI.handle { _ =>
      UIO {
        List(
          User(UUID.randomUUID(), "kit@email.com"),
          User(UUID.randomUUID(), "another@gmail.com")
        )
      }
    }

  val userHandler =
    userAPI.handle { id =>
      UIO.some(User(id, "kit@email.com"))
    }

  val countHandler =
    countAPI.handle { _ =>
      Counter.count
    }

  val incrementHandler =
    incrementAPI.handle { int =>
      Counter.increment(int)
    }

  val port = 9898
  val host = s"http://localhost:$port"

  private val apis     = userAPI ++ usersAPI ++ countAPI ++ incrementAPI
  private val handlers = userHandler ++ usersHandler ++ countHandler ++ incrementHandler

  val serverLayer =
    Server.start(port, apis, handlers).fork.unit.toLayer

  def spec: ZSpec[TestEnvironment, Any] =
    suite("ServerClientSpec")(
      test("get users") {
        for {
          users <- usersAPI.call(host)(())
        } yield assertTrue(users.length == 2)
      },
      test("get user") {
        for {
          _     <- ZIO.service[Unit]
          userId = UUID.randomUUID()
          user  <- userAPI.call(host)(userId)
        } yield assertTrue(user.get.id == userId)
      },
      test("counter api") {
        for {
          count  <- countAPI.call(host)(())
          _      <- incrementAPI.call(host)(2) <&> incrementAPI.call(host)(4)
          count2 <- countAPI.call(host)(())
        } yield assertTrue(count == 0 && count2 == 6)
      }
    ).provideCustom(
      EventLoopGroup.auto(),
      ChannelFactory.auto,
      serverLayer,
      Counter.live
    )

  // Example Service

  final case class Counter(ref: Ref[Int]) {
    def increment(amount: Int): ZIO[Any, Nothing, Unit] = ref.update(_ + amount)
    def count: ZIO[Any, Nothing, Int]                   = ref.get
  }

  object Counter {
    def increment(amount: Int): ZIO[Counter, Nothing, Unit] =
      ZIO.serviceWithZIO[Counter](_.increment(amount))

    val count: ZIO[Counter, Nothing, Int] =
      ZIO.serviceWithZIO[Counter](_.count)

    val live: ULayer[Counter] =
      Ref.make(0).toLayer >>> (Counter.apply _).toLayer
  }

  // User

  final case class User(id: UUID, email: String)

  object User {
    implicit val codec: JsonCodec[User] = DeriveJsonCodec.gen
    implicit val schema: Schema[User]   = DeriveSchema.gen
  }
}
