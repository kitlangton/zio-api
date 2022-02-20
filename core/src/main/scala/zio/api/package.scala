package zio

import zhttp.http.HttpApp
import zhttp.service.{ChannelFactory, Client, EventLoopGroup}
import zio.json.{DecoderOps, JsonCodec, JsonDecoder}
import zio.api.API.NotUnit
import zio.json.internal.{RetractReader, Write}

import java.util.UUID
import scala.language.implicitConversions

package object api {

  // Paths
  val string: Path[String]   = Path.MatchParser("string", Parser.stringParser)
  val int: Path[Int]         = Path.MatchParser("int", Parser.intParser)
  val boolean: Path[Boolean] = Path.MatchParser("boolean", Parser.booleanParser)
  val uuid: Path[UUID]       = Path.MatchParser("uuid", Parser.uuidParser)

  // Query Params
  def string(name: String): Query[String]   = Query.SingleParam(name, Parser.stringParser)
  def int(name: String): Query[Int]         = Query.SingleParam(name, Parser.intParser)
  def boolean(name: String): Query[Boolean] = Query.SingleParam(name, Parser.booleanParser)

  implicit def stringToPath(string: String): Path[Unit] = Path.path(string)

  // API Ops

  implicit def apiToOps[Params, Input, Output: NotUnit, ZipOut](
      api: API[Params, Input, Output]
  )(implicit zipper: Zipper.WithOut[Params, Input, ZipOut]): APIOps[Params, Input, Output, ZipOut, api.Id] =
    new APIOps(api)

  implicit def apiToOpsUnit[Params, Input, ZipOut](
      api: API[Params, Input, Unit]
  )(implicit zipper: Zipper.WithOut[Params, Input, ZipOut]): APIOpsUnit[Params, Input, ZipOut, api.Id] =
    new APIOpsUnit(api)

  final class APIOps[Params, Input, Output: NotUnit, ZipOut, Id](
      val self: API.WithId[Params, Input, Output, Id]
  )(implicit
      zipper: Zipper.WithOut[Params, Input, ZipOut]
  ) {
    def handle[R, E](
        f: ZipOut => ZIO[R, E, Output]
    ): Handler.WithId[R, E, Id] =
      Handler.make[R, E, Params, Input, Output](self) { case (params, input) =>
        f(zipper.zip(params, input))
      }

    def toHttp[R, E](
        f: ZipOut => ZIO[R, E, Output]
    ): HttpApp[R, E] =
      handle[R, E](f).toHttp

    def call(host: String)(params: ZipOut): ZIO[EventLoopGroup with ChannelFactory, Throwable, Output] = {
      val tuple = zipper.unzip(params)
      ClientInterpreter.interpret(host)(self)(tuple._1, tuple._2).flatMap(_.bodyAsString).flatMap { string =>
        self.outputCodec.decodeJson(string) match {
          case Left(err)    => ZIO.fail(new Error(s"Could not parse response: $err"))
          case Right(value) => ZIO.succeed(value)
        }
      }
    }
  }

  final class APIOpsUnit[Params, Input, ZipOut, Id](val self: API.WithId[Params, Input, Unit, Id])(implicit
      zipper: Zipper.WithOut[Params, Input, ZipOut]
  ) {
    def handle[R, E, Output2](
        f: ZipOut => ZIO[R, E, Output2]
    )(implicit
        codec: JsonCodec[Output2]
    ): Handler.WithId[R, E, Id] =
      Handler
        .make[R, E, Params, Input, Output2](self.output[Output2]) { case (params, input) =>
          f(zipper.zip(params, input))
        }
        .asInstanceOf[Handler.WithId[R, E, Id]]

    def toHttp[R, E, Output2](
        f: ZipOut => ZIO[R, E, Output2]
    )(implicit
        codec: JsonCodec[Output2]
    ): HttpApp[R, E] =
      handle[R, E, Output2](f).toHttp

    def call(host: String)(params: ZipOut): ZIO[EventLoopGroup with ChannelFactory, Throwable, Unit] = {
      val tuple = zipper.unzip(params)
      ClientInterpreter.interpret(host)(self)(tuple._1, tuple._2).unit
    }
  }

  lazy implicit val unitCodec: JsonCodec[Unit] = JsonCodec(
    (a: Unit, indent: Option[Int], out: Write) => (),
    (trace: List[JsonDecoder.JsonError], in: RetractReader) => ()
  )

}
