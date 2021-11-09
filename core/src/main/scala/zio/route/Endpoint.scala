package zio.route

import zio.json.internal.{RetractReader, Write}
import zio.json.{EncoderOps, JsonCodec, JsonDecoder, JsonEncoder}
import zio.{UIO, ZIO, Zippable, route}
import zio.schema.Schema

object EndpointParser {
  import zhttp.http.{Endpoint => _, _}

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

      case Map(info, f) =>
        parseRequest(info)(request).map(a => f(a))

      case queryParams: QueryParams[_] =>
        QueryParams.parse(queryParams, request.url.queryParams)

      case headers: Headers[_] =>
        Headers.parse(headers, request.headers)

      case route: route.Route[_] =>
        Route.parse(route, request.url.path.toList)

    }

  final case class UnapplyParser[A](f: Request => Option[A]) {
    def unapply(request: Request): Option[A] = f(request)
  }

  def interpret[R, E, Params, Input, Output](handler: Handler[R, E, Params, Input, Output]): HttpApp[R, E] = {
    val parser                                      = UnapplyParser(parseRequest(handler.endpoint.requestParser)(_))
    implicit val outputEncoder: JsonEncoder[Output] = handler.endpoint.response

    HttpApp.collectM { //
      case parser(result) =>
        ZIO.debug(s"RECEIVED: $result") *>
          handler
            .handle((result, ().asInstanceOf[Input])) // TODO: remove asInstanceOf
            .map(a => Response.jsonString(a.toJson))
    }
  }
}

final case class Endpoint[Params, Input, Output](
    method: HttpMethod,
    requestParser: RequestParser[Params], // Route / Params / Headers
    doc: Doc,
    request: JsonCodec[Input],
    response: JsonCodec[Output]
) { self =>
  type Id

  def query[A](queryParams: QueryParams[A])(implicit
      zippable: Zippable[Params, A]
  ): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ queryParams)

  def header[A](headers: Headers[A])(implicit zippable: Zippable[Params, A]): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ headers)

  def streamingInput: Endpoint[Params, Input, Output] =
    ???

  def withRequest[Input2]: Endpoint[Params, Input2, Output] =
    ???

  def withResponse[Output2](implicit codec: JsonCodec[Output2]): Endpoint[Params, Input, Output2] =
    copy(response = codec)

  //  def ??(doc: Doc): Endpoint[P, I, O] = ???

  def ++(that: Endpoint[_, _, _]): Endpoints[Id with that.Id] =
    Endpoints(self) ++ that
}

object Endpoint {

  /** Creates an endpoint for DELETE request at the given route.
    */
  def delete[A](route: Route[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.DELETE, route)

  /** Creates an endpoint for a GET request at the given route.
    */
  def get[A](route: Route[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.GET, route)

  /** Creates an endpoint for a POST request at the given route.
    */
  def post[A](route: Route[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.POST, route)

  /** Creates an endpoint for a PUT request at the given route.
    */
  def put[A](route: Route[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.PUT, route)

  /** Creates an endpoint with the given method and route.
    */
  private def method[Params](method: HttpMethod, route: Route[Params]): Endpoint[Params, Unit, Unit] =
    Endpoint(method, route, Doc.empty, unitCodec, unitCodec)

  lazy val unitCodec: JsonCodec[Unit] = JsonCodec(
    (a: Unit, indent: Option[Int], out: Write) => (),
    (trace: List[JsonDecoder.JsonError], in: RetractReader) => ()
  )
}
