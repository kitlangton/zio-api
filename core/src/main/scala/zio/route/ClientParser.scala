package zio.route

import zhttp.http.HttpData
import zhttp.service.{ChannelFactory, Client, EventLoopGroup}
import zio.ZIO

object ClientParser {
  def request[Params, Input, Output](host: String)(
      endpoint: Endpoint[Params, Input, Output]
  )(params: Params, input: Input): ZIO[EventLoopGroup with ChannelFactory, Throwable, Client.ClientResponse] = {
    val method         = endpoint.method.toZioHttpMethod
    val (url, headers) = parseUrl(endpoint.requestParser)(params)
    val data =
      if (endpoint.inputCodec == Endpoint.unitCodec) HttpData.empty
      else HttpData.fromString(endpoint.inputCodec.encodeJson(input, None).toString)
    Client.request(s"$host$url", method, zhttp.http.Headers(headers.toList), content = data)
  }

  def parseUrl[Params](
      requestParser: RequestParser[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    requestParser match {
      case RequestParser.Zip(left, right) =>
        params match {
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

  def parsePath[Params](
      route: Path[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    route match {
      case Path.MapPath(route, _, g) =>
        parsePath(route)(g(params))
      case Path.Zip(left, right) =>
        params match {
          case (a, b) =>
            val (l, r)   = parsePath(left)(a)
            val (l2, r2) = parsePath(right)(b)
            (l + l2, r ++ r2)
        }
      case Path.MatchLiteral(literal) =>
        ("/" + literal, scala.Predef.Map.empty)
      case Path.End =>
        "/" -> scala.Predef.Map.empty
      case Path.MatchParser(_, _) =>
        ("/" + params, scala.Predef.Map.empty)
    }

  def parseQuery[Params](
      query: Query[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    query match {
      case Query.SingleParam(name, parser) =>
        ("?" + name + "=" + params.toString, scala.Predef.Map.empty)
      case Query.Optional(p) =>
        params match {
          case Some(params) =>
            parseQuery(p)(params)
          case None =>
            "" -> scala.Predef.Map.empty
        }

      case Query.Zip(left, right) =>
        val (l, r)   = parseQuery(left)(params)
        val (l2, r2) = parseQuery(right)(params)
        (l + l2, r ++ r2)

      case Query.MapParams(info, _, g) =>
        parseQuery(info)(g(params))
    }

  def parseHeaders[Params](
      headers: Header[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    headers match {
      case Header.Map(headers, _, g) =>
        parseHeaders(headers)(g(params))
      case Header.Zip(left, right) =>
        val (l, r)   = parseHeaders(left)(params)
        val (l2, r2) = parseHeaders(right)(params)
        (l + l2, r ++ r2)
      case Header.Optional(headers) =>
        parseHeaders(headers)(params)
      case Header.SingleHeader(name, parser) =>
        ("", scala.Predef.Map(name -> params.toString))
    }
}
