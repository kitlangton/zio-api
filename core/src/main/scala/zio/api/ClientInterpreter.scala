package zio.api

import zhttp.http.HttpData
import zhttp.service.{ChannelFactory, Client, EventLoopGroup}
import zio.ZIO

import scala.collection.mutable

private[api] object ClientInterpreter {
  def interpret[Params, Input, Output](host: String)(
      api: API[Params, Input, Output]
  )(params: Params, input: Input): ZIO[EventLoopGroup with ChannelFactory, Throwable, Client.ClientResponse] = {
    val method = api.method.toZioHttpMethod
    val state  = new RequestState()
    parseUrl(api.requestParser, state)(params)
    val (url, headers) = state.result
    val data =
      if (api.inputCodec == unitCodec) HttpData.empty
      else HttpData.fromString(api.inputCodec.encodeJson(input, None).toString)
    Client.request(s"$host$url", method, zhttp.http.Headers(headers.toList), content = data)
  }

  private[api] class RequestState {
    private val query: mutable.Map[String, String]   = mutable.Map.empty
    private val headers: mutable.Map[String, String] = mutable.Map.empty
    private val pathBuilder: StringBuilder           = new StringBuilder()

    def addPath(path: String): Unit =
      pathBuilder.addAll(path)

    def addQuery(key: String, value: String): Unit =
      query.put(key, value)

    def addHeader(key: String, value: String): Unit =
      headers.put(key, value)

    def result: (String, Map[String, String]) = {

      val queryString =
        if (query.nonEmpty)
          query.map { case (key, value) => s"$key=$value" }.mkString("?", "&", "")
        else
          ""

      (pathBuilder.result + queryString, headers.toMap)
    }
  }

  private[api] def parseUrl[Params](
      requestParser: RequestParser[Params],
      state: RequestState
  )(params: Params): Unit =
    requestParser match {
      case RequestParser.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            parseUrl(left, state)(a)
            parseUrl(right, state)(b)
        }
      case RequestParser.Map(info, _, g) =>
        parseUrl(info, state)(g(params))

      case headers: Header[_] =>
        parseHeaders[Params](headers, state)(params)

      case query: Query[_] =>
        parseQuery[Params](query, state)(params)

      case route: Path[_] =>
        parsePath[Params](route, state)(params)
    }

  private def parsePath[Params](
      route: Path[Params],
      state: RequestState
  )(params: Params): Unit =
    route match {
      case Path.MapPath(route, _, g) =>
        parsePath(route, state)(g(params))
      case Path.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            parsePath(left, state)(a)
            parsePath(right, state)(b)
        }
      case Path.MatchLiteral(literal) =>
        state.addPath("/" + literal)
      case Path.End =>
        ()
      case Path.MatchParser(_, _, _) =>
        state.addPath("/" + params.toString)
    }

  private def parseQuery[Params](
      query: Query[Params],
      state: RequestState
  )(params: Params): Unit =
    query match {
      case Query.SingleParam(name, parser, _) =>
        state.addQuery(name, params.toString)
      case Query.Optional(p) =>
        params match {
          case Some(params) =>
            parseQuery(p, state)(params)
          case None =>
            ()
        }

      case Query.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            parseQuery(left, state)(a)
            parseQuery(right, state)(b)
        }

      case Query.MapParams(info, _, g) =>
        parseQuery(info, state)(g(params))
    }

  private def parseHeaders[Params](
      headers: Header[Params],
      state: RequestState
  )(params: Params): Unit =
    headers match {
      case Header.Map(headers, _, g) =>
        parseHeaders(headers, state)(g(params))
      case Header.ZipWith(left, right, _, g) =>
        g(params) match {
          case (a, b) =>
            parseHeaders(left, state)(a)
            parseHeaders(right, state)(b)
        }
      case Header.Optional(headers) =>
        params match {
          case Some(params) =>
            parseHeaders(headers, state)(params)
          case None =>
            ()
        }
      case Header.SingleHeader(name, parser) =>
        state.addHeader(name, params.toString)
    }
}
