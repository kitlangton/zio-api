package zio.api

import zhttp.http.HttpData
import zhttp.service.{ChannelFactory, Client, EventLoopGroup}
import zio.ZIO

private[api] object ClientInterpreter {
  def interpret[Params, Input, Output](host: String)(
      api: API[Params, Input, Output]
  )(params: Params, input: Input): ZIO[EventLoopGroup with ChannelFactory, Throwable, Client.ClientResponse] = {
    val method         = api.method.toZioHttpMethod
    val (url, headers) = parseUrl(api.requestParser)(params)
    val data =
      if (api.inputCodec == unitCodec) HttpData.empty
      else HttpData.fromString(api.inputCodec.encodeJson(input, None).toString)
    Client.request(s"$host$url", method, zhttp.http.Headers(headers.toList), content = data)
  }

  private def parseUrl[Params](
      requestParser: RequestParser[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    requestParser match {
      case RequestParser.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            val (l, r)   = parseUrl(left)(a)
            val (l2, r2) = parseUrl(right)(b)
            (l + l2, r ++ r2)
        }
      case RequestParser.Map(info, _, g) =>
        parseUrl(info)(g(params))

      case headers: Header[_] =>
        parseHeaders[Params](headers)(params)

      case query: Query[_] =>
        parseQuery[Params](query)(params)

      case route: Path[_] =>
        parsePath[Params](route)(params)
    }

  private def parsePath[Params](
      route: Path[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    route match {
      case Path.MapPath(route, _, g) =>
        parsePath(route)(g(params))
      case Path.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            val (l, r)   = parsePath(left)(a)
            val (l2, r2) = parsePath(right)(b)
            (l + l2, r ++ r2)
        }
      case Path.MatchLiteral(literal) =>
        ("/" + literal, scala.Predef.Map.empty)
      case Path.End =>
        "/" -> scala.Predef.Map.empty
      case Path.MatchParser(_, _, _) =>
        ("/" + params, scala.Predef.Map.empty)
    }

  private def parseQuery[Params](
      query: Query[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    query match {
      case Query.SingleParam(name, parser, _) =>
        ("?" + name + "=" + params.toString, scala.Predef.Map.empty)
      case Query.Optional(p) =>
        params match {
          case Some(params) =>
            parseQuery(p)(params)
          case None =>
            "" -> scala.Predef.Map.empty
        }

      case Query.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            val (l, r)   = parseQuery(left)(a)
            val (l2, r2) = parseQuery(right)(b)
            (l + l2, r ++ r2)
        }

      case Query.MapParams(info, _, g) =>
        parseQuery(info)(g(params))
    }

  private def parseHeaders[Params](
      headers: Header[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    headers match {
      case Header.Map(headers, _, g) =>
        parseHeaders(headers)(g(params))
      case Header.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            val (l, r)   = parseHeaders(left)(a)
            val (l2, r2) = parseHeaders(right)(b)
            (l + l2, r ++ r2)
        }
      case Header.Optional(headers) =>
        parseHeaders(headers)(params)
      case Header.SingleHeader(name, parser) =>
        ("", scala.Predef.Map(name -> params.toString))
    }
}
