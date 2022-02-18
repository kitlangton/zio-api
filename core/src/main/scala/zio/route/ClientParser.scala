package zio.route

import zhttp.service.Client

object ClientParser {
  def request[Params, Input, Output](endpoint: Endpoint[Params, Input, Output])(params: Params) = {
    val method         = endpoint.method.toZioHttpMethod
    val (url, headers) = parseUrl(endpoint.requestParser)(params)
    Client.request(s"http://localhost:8080$url", method, zhttp.http.Headers(headers.toList))
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

      case headers: Headers[_] =>
        parseHeaders[Params](headers)(params)

      case query: QueryParams[_] =>
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
      query: QueryParams[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    query match {
      case QueryParams.SingleParam(name, parser) =>
        ("?" + name + "=" + params.toString, scala.Predef.Map.empty)
      case QueryParams.Optional(p) =>
        params match {
          case Some(params) =>
            parseQuery(p)(params)
          case None =>
            "" -> scala.Predef.Map.empty
        }

      case QueryParams.Zip(left, right) =>
        val (l, r)   = parseQuery(left)(params)
        val (l2, r2) = parseQuery(right)(params)
        (l + l2, r ++ r2)

      case QueryParams.MapParams(info, _, g) =>
        parseQuery(info)(g(params))
    }

  def parseHeaders[Params](
      headers: Headers[Params]
  )(params: Params): (String, scala.Predef.Map[String, String]) =
    headers match {
      case Headers.Map(headers, _, g) =>
        parseHeaders(headers)(g(params))
      case Headers.Zip(left, right) =>
        val (l, r)   = parseHeaders(left)(params)
        val (l2, r2) = parseHeaders(right)(params)
        (l + l2, r ++ r2)
      case Headers.Optional(headers) =>
        parseHeaders(headers)(params)
      case Headers.SingleHeader(name, parser) =>
        ("", scala.Predef.Map(name -> params.toString))
    }
}
