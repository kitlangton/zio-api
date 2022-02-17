package zio.route

import zhttp.service.Client
import zio.json.internal.{RetractReader, Write}
import zio.json._
import zio.{UIO, ZIO, Zippable, route}
import zio.schema.Schema
import zhttp.http.{Headers => _, _}

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

      case Map(info, f) =>
        parseRequest(info)(request).map(a => f(a))

      case queryParams: QueryParams[_] =>
        QueryParams.parse(queryParams, request.url.queryParams)

      case headers: route.Headers[_] =>
        route.Headers.parse(headers, request.headers.toList)

      case route: route.Route[_] =>
        Route.parse(route, request.url.path.toList)

    }

  def interpret[R, E, Params, Input, Output](handler: Handler[R, E, Params, Input, Output]): HttpApp[R, E] = {
    val parser: PartialFunction[Request, Params]    = (parseRequest(handler.endpoint.requestParser)(_)).unlift
    implicit val outputEncoder: JsonEncoder[Output] = handler.endpoint.response.encoder
    implicit val inputDecoder: JsonDecoder[Input]   = handler.endpoint.request.decoder

    Http.collectZIO { //
      case req @ parser(result) =>
//        req.getBodyAsString.flatMap(_.fromJson[Input])

        ZIO.debug(s"RECEIVED: $result") *>
          handler
            .handle((result, ().asInstanceOf[Input])) // TODO: remove asInstanceOf
            .map(a => Response.json(a.toJson))
    }
  }

  def request[Params, Input, Output](endpoint: Endpoint[Params, Input, Output]) = {
    val method = endpoint.method
    val url    = endpoint.requestParser
//    Client.request((method, url))
    ???
  }

  def parseUrl[Params](
      requestParser: RequestParser[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) = {
    println("\n---")
    println(requestParser)
    println(params)
    requestParser match {
      case Zip(left, right) =>
        params match {
          case (a, b) =>
            val (l, r)   = parseUrl(left)(a)
            val (l2, r2) = parseUrl(right)(b)
            (l + l2, r ++ r2)

          case other =>
            val (l, r)   = parseUrl(left)(other)
            val (l2, r2) = parseUrl(right)(other)
            (l + l2, r ++ r2)

        }
      case Map(info, f) =>
        parseUrl(info)(params)
      case headers: Headers[_] =>
        headers match {
          case Headers.Map(headers, f) =>
            parseUrl(headers)(f(params))
          case Headers.Zip(left, right) =>
            val (l, r)   = parseUrl(left)(params)
            val (l2, r2) = parseUrl(right)(params)
            (l + l2, r ++ r2)
          case Headers.Optional(headers) =>
            parseUrl(headers)(params)
          case Headers.SingleHeader(name, parser) =>
            ("", scala.Predef.Map(name -> params.toString))
        }
      case p: QueryParams[_] =>
        p match {
          case QueryParams.SingleParam(name, parser) =>
            (s"?$name=$params", scala.Predef.Map.empty)
          case QueryParams.Optional(p) =>
            params match {
              case Some(params) =>
                parseUrl(p)(params)
              case None =>
                "" -> scala.Predef.Map.empty
            }
        }
      case route: Route[_] =>
        route match {
          case Route.MapRoute(route, f) =>
            parseUrl(route)(f(params))

          case Route.Zip(left, right) =>
            val (l, r)   = parseUrl(left)(params)
            val (l2, r2) = parseUrl(right)(params)
            (l + l2, r ++ r2)

          case Route.MatchLiteral(literal) =>
            ("/" + literal, scala.Predef.Map.empty)

          case Route.End =>
            "/" -> scala.Predef.Map.empty

          case Route.MatchParser(name, parser) =>
            ("/" + name, scala.Predef.Map.empty)
        }
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
