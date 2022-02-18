package zio.route

import zhttp.http.{Headers => _, Path => _, _}
import zio.json._
import zio.json.internal.{RetractReader, Write}
import zio.route.Endpoint.{NotUnit, unitCodec}
import zio.route.Handler.WithId
import zio.{UIO, ZIO, route}

object EndpointParser {

  import RequestParser._
  // Route[A] => List[String] => Option[A]
  // Headers[A] => Map[String, String] => Option[A]
  // QueryParams[A] => Map[String, List[String]] => Option[A]
  // RequestParser[A] => (Request => Option[A])
  def parseRequest[A](requestInfo: RequestParser[A])(request: Request): Option[A] =
    requestInfo match {
      case Zip(left, right) =>
        for {
          a <- parseRequest(left)(request)
          b <- parseRequest(right)(request)
        } yield (a, b)

      case Map(info, f, _) =>
        parseRequest(info)(request).map(a => f(a))

      case queryParams: QueryParams[_] =>
        QueryParams.parse(queryParams, request.url.queryParams)

      case headers: route.Headers[_] =>
        route.Headers.parse(headers, request.headers.toList)

      case route: route.Path[_] =>
        Path.parse(route, request.url.path.toList)
    }

  def handlerToHttpApp[R, E, Params, Input, Output](
      handler: HandlerImpl[R, E, Params, Input, Output]
  ): HttpApp[R, E] = {
    val parser: PartialFunction[Request, Params]    = (parseRequest(handler.endpoint.requestParser)(_)).unlift
    implicit val outputEncoder: JsonEncoder[Output] = handler.endpoint.outputCodec.encoder
    implicit val inputDecoder: JsonDecoder[Input]   = handler.endpoint.inputCodec.decoder

    def withInput(request: Request)(process: Input => ZIO[R, E, Response]): ZIO[R, E, Response] =
      if (handler.endpoint.inputCodec == unitCodec) {
        process(().asInstanceOf[Input])
      } else {
        request.bodyAsString
          .flatMap { string =>
            string.fromJson[Input] match {
              case Left(err)    => UIO(Response.text(s"Invalid input: $err"))
              case Right(value) => process(value)
            }
          }
          .catchAll { err =>
            UIO(Response.text(s"Error parsing request body: $err"))
          }
      }

    Http.collectZIO { //
      case req @ parser(result) if req.method == handler.endpoint.method.toZioHttpMethod =>
        ZIO.debug(s"RECEIVED: $result") *>
          withInput(req) { input =>
            handler
              .handle((result, input)) // TODO: remove asInstanceOf
              .map { a =>
                if (handler.endpoint.outputCodec == unitCodec) {
                  Response.ok
                } else {
                  Response.json(a.toJson)
                }
              }
          }
    }
  }

}

final case class Endpoint[Params, Input, Output](
    method: HttpMethod,
    requestParser: RequestParser[Params], // Path / Params / Headers
    doc: Doc,
    inputCodec: JsonCodec[Input],
    outputCodec: JsonCodec[Output]
) { self =>
  type Id

  def query[A](queryParams: QueryParams[A])(implicit
      zippable: Zipper[Params, A]
  ): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ queryParams)

  def header[A](headers: Headers[A])(implicit zippable: Zipper[Params, A]): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ headers)

  def input[Input2](implicit codec: JsonCodec[Input2]): Endpoint[Params, Input2, Output] =
    copy(inputCodec = codec)

  def output[Output2](implicit codec: JsonCodec[Output2]): Endpoint[Params, Input, Output2] =
    copy(outputCodec = codec)

  def ++(that: Endpoint[_, _, _]): Endpoints[Id with that.Id] =
    Endpoints(self) ++ that
}

object Endpoint {

  trait NotUnit[A]

  object NotUnit {
    implicit def notUnit[A]: NotUnit[A] = new NotUnit[A] {}

    implicit val notUnitUnit1: NotUnit[Unit] = new NotUnit[Unit] {}
    implicit val notUnitUnit2: NotUnit[Unit] = new NotUnit[Unit] {}
  }

  implicit final class EndpointOps[Params, Input, Output: NotUnit, ZipOut](val self: Endpoint[Params, Input, Output])(
      implicit zipper: Zipper.Out[Params, Input, ZipOut]
  ) {
    def handle[R, E](
        f: ZipOut => ZIO[R, E, Output]
    ): WithId[R, E, self.Id] =
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
    ): WithId[R, E, self.Id] =
      Handler
        .make[R, E, Params, Input, Output2](self.output[Output2]) { case (params, input) =>
          f(zipper.zip(params, input))
        }
        .asInstanceOf[WithId[R, E, self.Id]]

    def toHttp[R, E, Output2](
        f: ZipOut => ZIO[R, E, Output2]
    )(implicit
        codec: JsonCodec[Output2]
    ) =
      handle[R, E, Output2](f).toHttp
  }

  /** Creates an endpoint for DELETE request at the given route.
    */
  def delete[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.DELETE, route)

  /** Creates an endpoint for a GET request at the given route.
    */
  def get[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.GET, route)

  /** Creates an endpoint for a POST request at the given route.
    */
  def post[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.POST, route)

  /** Creates an endpoint for a PUT request at the given route.
    */
  def put[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.PUT, route)

  /** Creates an endpoint with the given method and route.
    */
  private def method[Params](method: HttpMethod, route: Path[Params]): Endpoint[Params, Unit, Unit] =
    Endpoint(method, route, Doc.empty, unitCodec, unitCodec)

  lazy implicit val unitCodec: JsonCodec[Unit] = JsonCodec(
    (a: Unit, indent: Option[Int], out: Write) => (),
    (trace: List[JsonDecoder.JsonError], in: RetractReader) => ()
  )
}
