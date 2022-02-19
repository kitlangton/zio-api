package zio

import zio.json.JsonCodec
import zio.route.Endpoint.NotUnit

import java.util.UUID
import scala.language.implicitConversions

package object route {

  // Routes
  val string: Path[String]   = Path.MatchParser("string", Parser.stringParser)
  val int: Path[Int]         = Path.MatchParser("int", Parser.intParser)
  val boolean: Path[Boolean] = Path.MatchParser("boolean", Parser.booleanParser)
  val uuid: Path[UUID]       = Path.MatchParser("uuid", Parser.uuidParser)

  // Query Params
  def string(name: String): Query[String]   = Query.SingleParam(name, Parser.stringParser)
  def int(name: String): Query[Int]         = Query.SingleParam(name, Parser.intParser)
  def boolean(name: String): Query[Boolean] = Query.SingleParam(name, Parser.booleanParser)

  implicit def string2Route(string: String): Path[Unit] = Path.path(string)

  // Endpoint Ops

  implicit final class EndpointOps[Params, Input, Output: NotUnit, ZipOut](
      val self: Endpoint[Params, Input, Output]
  )(implicit
      zipper: Zipper.Out[Params, Input, ZipOut]
  ) {
    def handle[R, E](
        f: ZipOut => ZIO[R, E, Output]
    ): Handler.WithId[R, E, self.Id] =
      Handler.make[R, E, Params, Input, Output](self) { case (params, input) =>
        f(zipper.zip(params, input))
      }

    def toHttp[R, E](
        f: ZipOut => ZIO[R, E, Output]
    ) =
      handle[R, E](f).toHttp
  }

  implicit final class EndpointOpsUnit[Params, Input, ZipOut](val self: Endpoint[Params, Input, Unit])(implicit
      zipper: Zipper.Out[Params, Input, ZipOut]
  ) {
    def handle[R, E, Output2](
        f: ZipOut => ZIO[R, E, Output2]
    )(implicit
        codec: JsonCodec[Output2]
    ): Handler.WithId[R, E, self.Id] =
      Handler
        .make[R, E, Params, Input, Output2](self.output[Output2]) { case (params, input) =>
          f(zipper.zip(params, input))
        }
        .asInstanceOf[Handler.WithId[R, E, self.Id]]

    def toHttp[R, E, Output2](
        f: ZipOut => ZIO[R, E, Output2]
    )(implicit
        codec: JsonCodec[Output2]
    ) =
      handle[R, E, Output2](f).toHttp
  }

}
