package zio.route

import zio.{UIO, ZIO, Zippable, route}
import zio.schema.Schema

sealed trait HttpMethod extends Product with Serializable

object EndpointParser {
  import zhttp.http.{Endpoint => _, _}

  import RequestParser._
  def parseRequest[A](requestInfo: RequestParser[A])(request: Request): Option[A] =
    requestInfo match {
      case zip: Zip[_, _] =>
        for {
          a <- parseRequest(zip.left)(request)
          b <- parseRequest(zip.right)(request)
        } yield (a, b).asInstanceOf[A]

      case Map(info, f) =>
        parseRequest(info)(request).map(a => f.asInstanceOf[Any => A](a))

      case queryParams: QueryParams[_] =>
        queryParams.parse(request).asInstanceOf[Option[A]]

      case headers: Headers[_] =>
        headers.parse(request).asInstanceOf[Option[A]]

      case route: route.Route[_] =>
        Route.parseImpl(request.url.path.toList, route).map(_._2).asInstanceOf[Option[A]]

    }

  final case class UnapplyParser[A](f: Request => Option[A]) {
    def unapply(request: Request): Option[A] = f(request)
  }

  def interpret[R, E, Params, Output](handler: Handler[R, E, Params, Unit, Output]): HttpApp[R, E] = {
    val parser = UnapplyParser(parseRequest(handler.endpoint.requestInfo)(_))

    HttpApp.collectM { case parser(result) =>
      ZIO.debug(s"RECEIVED: $result") *>
        handler
          .handler((result, ()))
          .map(a => Response.text(a.toString))
    }
  }
}

object HttpMethod {
  case object GET    extends HttpMethod
  case object POST   extends HttpMethod
  case object PATCH  extends HttpMethod
  case object PUT    extends HttpMethod
  case object DELETE extends HttpMethod
}

final case class Endpoint[Params, Input, Output](
    method: HttpMethod,
    requestInfo: RequestParser[Params],
    doc: Doc,
    request: Schema[Input],
    response: Schema[Output]
) { self =>
  type Id

  def query[A](queryParams: QueryParams[A])(implicit
      zippable: Zippable[Params, A]
  ): Endpoint[zippable.Out, Input, Output] =
    copy(requestInfo = requestInfo ++ queryParams)

  def header[A](headers: Headers[A])(implicit zippable: Zippable[Params, A]): Endpoint[zippable.Out, Input, Output] =
    copy(requestInfo = requestInfo ++ headers)

  def streamingInput: Endpoint[Params, Input, Output] =
    ???

  def withRequest[Input2]: Endpoint[Params, Input2, Output] =
    ???

  def withResponse[Output2]: Endpoint[Params, Input, Output2] =
    self.asInstanceOf[Endpoint[Params, Input, Output2]]

  //  def ??(doc: Doc): Endpoint[P, I, O] = ???

  def ++(that: Endpoint[_, _, _]): Endpoints[Id with that.Id] =
    Endpoints(self) ++ that
}

object Endpoint {

  /** Creates an endpoint for a GET request at the given route.
    */
  def get[A](route: Route[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.GET, route)

  /** Creates an endpoint for a POST request at the given route.
    */
  def post[A](route: Route[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.POST, route)

  /** Creates an endpoint with the given method and route.
    */
  private def method[Params](method: HttpMethod, route: Route[Params]): Endpoint[Params, Unit, Unit] =
    Endpoint(method, route, Doc.empty, Schema[Unit], Schema[Unit])
}
