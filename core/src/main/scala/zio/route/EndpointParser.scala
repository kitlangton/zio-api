package zio.route

import zhttp.http.{Header => HttpHeader, Http, HttpApp, Request, Response}
import zio.json._
import zio.{UIO, ZIO}

object EndpointParser {

  private[route] def handlerToHttpApp[R, E, Params, Input, Output](
      handler: HandlerImpl[R, E, Params, Input, Output]
  ): HttpApp[R, E] = {
    val parser: PartialFunction[Request, Params]    = (handler.endpoint.requestParser.parseRequest _).unlift
    implicit val outputEncoder: JsonEncoder[Output] = handler.endpoint.outputCodec.encoder
    implicit val inputDecoder: JsonDecoder[Input]   = handler.endpoint.inputCodec.decoder

    def withInput(request: Request)(process: Input => ZIO[R, E, Response]): ZIO[R, E, Response] =
      if (handler.endpoint.inputCodec == Endpoint.unitCodec) {
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
                if (handler.endpoint.outputCodec == Endpoint.unitCodec) {
                  Response.ok
                } else {
                  Response.json(a.toJson)
                }
              }
          }
    }
  }

  private[route] def parseRequest[A](requestInfo: RequestParser[A])(request: Request): Option[A] =
    requestInfo match {
      case RequestParser.Zip(left, right) =>
        for {
          a <- parseRequest(left)(request)
          b <- parseRequest(right)(request)
        } yield (a, b)

      case RequestParser.Map(info, f, _) =>
        parseRequest(info)(request).map(a => f(a))

      case queryParams: Query[_] =>
        parseQueryParams(queryParams, request.url.queryParams)

      case headers: Header[_] =>
        parseHeaders(headers, request.headers.toList)

      case route: Path[_] =>
        parsePath(route, request.url.path.toList)
    }

  private[route] def parseQueryParams[A](
      queryParams: Query[A],
      requestParams: Map[String, List[String]]
  ): Option[A] =
    queryParams match {
      case Query.SingleParam(name, parser) =>
        requestParams.get(name).flatMap(_.headOption).flatMap(parser.parse)

      case Query.Zip(left, right) =>
        for {
          l <- parseQueryParams(left, requestParams)
          r <- parseQueryParams(right, requestParams)
        } yield (l, r).asInstanceOf[A]

      case Query.Optional(params) =>
        Some(parseQueryParams(params, requestParams)).asInstanceOf[Option[A]]

      case Query.MapParams(info, f, _) =>
        parseQueryParams(info, requestParams).map(f.asInstanceOf[Any => A])
    }

  private[route] def parseHeaders[A](headers: Header[A], requestHeaders: List[HttpHeader]): Option[A] =
    headers match {
      case Header.SingleHeader(name, parser) =>
        requestHeaders.collectFirst { case (`name`, value) =>
          parser.parse(value.toString)
        }.flatten

      case Header.Zip(left, right) =>
        for {
          left  <- parseHeaders(left, requestHeaders)
          right <- parseHeaders(right, requestHeaders)
        } yield (left, right)

      case Header.Optional(headers) =>
        Some(parseHeaders(headers, requestHeaders))

      case Header.Map(headers, f, _) =>
        parseHeaders(headers, requestHeaders).map(f.asInstanceOf[Any => A])
    }

  private[route] def parsePath[A](route: Path[A], input: List[String]): Option[A] =
    parsePathImpl(route, input).map(_._2)

  private[route] def parsePathImpl[A](route: Path[A], input: List[String]): Option[(List[String], A)] =
    route match {
      case Path.MatchLiteral(string) =>
        if (input.headOption.contains(string)) Some(input.tail -> ())
        else None

      case Path.MatchParser(_, parser) =>
        input.headOption.flatMap { head =>
          parser
            .parse(head)
            .map(input.tail -> _)
        }

      case Path.Zip(left, right) =>
        for {
          (input0, a) <- parsePathImpl(left, input)
          (input1, b) <- parsePathImpl(right, input0)
        } yield (input1, (a, b))

      case Path.MapPath(route, f, _) =>
        parsePathImpl(route, input)
          .map { case (input, output) =>
            (input, f(output))
          }

      case Path.End =>
        if (input.isEmpty) Some(input -> ())
        else None
    }

}
