package zio.route

import zhttp.http.{Headers => _, Path => _, _}
import zio.json._
import zio.json.internal.{RetractReader, Write}
import zio.{ZIO, Zippable, route}

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

  def interpret[R, E, Params, Input, Output](handler: Handler[R, E, Params, Input, Output]): HttpApp[R, E] = {
    val parser: PartialFunction[Request, Params]    = (parseRequest(handler.endpoint.requestParser)(_)).unlift
    implicit val outputEncoder: JsonEncoder[Output] = handler.endpoint.output.encoder
    implicit val inputDecoder: JsonDecoder[Input]   = handler.endpoint.input.decoder

    Http.collectZIO { //
      case req @ parser(result) =>
//        req.getBodyAsString.flatMap(_.fromJson[Input])

        ZIO.debug(s"RECEIVED: $result") *>
          handler
            .handle((result, ().asInstanceOf[Input])) // TODO: remove asInstanceOf
            .map(a => Response.json(a.toJson))
    }
  }

}

final case class Endpoint[Params, Input, Output](
    method: HttpMethod,
    requestParser: RequestParser[Params], // Route / Params / Headers
    doc: Doc,
    input: JsonCodec[Input],
    output: JsonCodec[Output]
) { self =>
  type Id

  def query[A](queryParams: QueryParams[A])(implicit
      zippable: Zipper[Params, A]
  ): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ queryParams)

  def header[A](headers: Headers[A])(implicit zippable: Zipper[Params, A]): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ headers)

  def withInput[Input2](implicit codec: JsonCodec[Input2]): Endpoint[Params, Input2, Output] =
    copy(input = codec)

  def withOutput[Output2](implicit codec: JsonCodec[Output2]): Endpoint[Params, Input, Output2] =
    copy(output = codec)

  def ++(that: Endpoint[_, _, _]): Endpoints[Id with that.Id] =
    Endpoints(self) ++ that
}

object Endpoint {

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

  lazy val unitCodec: JsonCodec[Unit] = JsonCodec(
    (a: Unit, indent: Option[Int], out: Write) => (),
    (trace: List[JsonDecoder.JsonError], in: RetractReader) => ()
  )
}
