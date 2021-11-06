package zio.route

import zhttp.service.Server
import zio._

import scala.language.implicitConversions

final case class Box[+A](name: String)

object RouteExample extends App {
  import Route._

  final case class Person(name: String, age: Int)

  val myEndpoint: Endpoint[(Person, (String, String), (Option[String], Option[Int])), Unit, Unit] =
    Endpoint
      .get(("dogs" / string / "cool" / int).map(Person.tupled))
      .header(Headers.UserAgent ++ Headers.AcceptEncoding)
      .query(QueryParams.string("name").? ++ QueryParams.int("age").?)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8888, EndpointParser.interpret(myEndpoint)).exitCode
}

sealed trait MimeType

sealed trait HeaderType[A]

object HeaderType {}

trait Doc

object Doc {
  val empty: Doc = Empty

  case object Empty extends Doc
}

trait Endpoints {}

// (getPosts ++ createPost).routed("/api")
// val userApi = EndpointAspect.at("/api/users")
// getPosts = (users / $userId / posts -> GET) @@ userApi
// createPost = (users / $userId / posts -> POST) @@ userApi
// client.invoke(createPost)
// - do we specify headers -> token (part of Params)
// - Endpoints()
// -

/** Http Concepts
  *   - verb
  *     - GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS, CONNECT, TRACE
  *   - path
  *   - query params
  *   - headers
  *   - input
  *     - body
  *   - output
  *
  * Differentiators between ZIO and Tapir
  *   - ZIO-native
  *   - Grouped endpoints
  */

// further nest Endpoints
//      .nest("/api")

/** sealed trait PayloadType { type Z[-_, +_, +_] }
  *
  * object PayloadType {
  *
  * // Duplicate this for input and output // so it's more readable in user's type signatures type Single = Single.type
  * case object Single extends PayloadType { type Z[-R, +E, +A] = ZIO[R, E, A] }
  *
  * type Streaming = Streaming.type case object Streaming extends PayloadType { type Z[-R, +E, +A] = ZStream[R, E, A] }
  * }
  */
