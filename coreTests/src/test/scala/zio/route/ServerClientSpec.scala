package zio.route

import zhttp.service.{ChannelFactory, EventLoopGroup}
import zio._
import zio.json.{uuid => _, _}
import zio.route.Endpoint.unitCodec
import zio.route.Handler.WithId
import zio.route.ServerClientSpec.userEndpoint
import zio.test._

import java.util.UUID

object ServerClientSpec extends DefaultRunnableSpec {

  // Endpoints

  val usersEndpoint =
    Endpoint
      .get("users")
      .output[List[User]]

  val countEndpoint =
    Endpoint
      .get("counter")
      .output[Int]

  val incrementEndpoint =
    Endpoint
      .post("counter")
      .input[Int]

  val userEndpoint =
    Endpoint
      .get("users" / uuid)
      .output[Option[User]]

  // Handlers

  val usersHandler =
    usersEndpoint.handle { _ =>
      UIO {
        List(
          User(UUID.randomUUID(), "kit@email.com"),
          User(UUID.randomUUID(), "another@gmail.com")
        )
      }
    }

  val userHandler =
    userEndpoint.handle { id =>
      UIO.some(User(id, "kit@email.com"))
    }

  val countHandler =
    countEndpoint.handle { _ =>
      Counter.count
    }

  val incrementHandler =
    incrementEndpoint.handle { int =>
      Counter.increment(int)
    }

  val port = 9898
  val host = s"http://localhost:$port"

  private val endpoints = userEndpoint ++ usersEndpoint ++ countEndpoint ++ incrementEndpoint

  private val handlers = userHandler ++ usersHandler ++ countHandler ++ incrementHandler

  val serverLayer =
    Server.start(port, endpoints, handlers).fork.unit.toLayer

  def spec: ZSpec[TestEnvironment, Any] =
    suite("ServerClientSpec")(
      test("get users") {
        for {
          users <- usersEndpoint.call(host)(())
        } yield assertTrue(users.length == 2)
      },
      test("get user") {
        for {
          _     <- ZIO.service[Unit]
          userId = UUID.randomUUID()
          user  <- userEndpoint.call(host)(userId)
        } yield assertTrue(user.get.id == userId)
      },
      test("counter routes") {
        for {
          count  <- countEndpoint.call(host)(())
          _      <- incrementEndpoint.call(host)(2) <&> incrementEndpoint.call(host)(4)
          count2 <- countEndpoint.call(host)(())
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
  }
}
