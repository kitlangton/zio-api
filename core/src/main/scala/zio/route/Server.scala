package zio.route

import zio.route.macros.Matches
import zio.ZIO

final case class Server[R, E <: Throwable](endpoints: Endpoints[_], handlers: Handlers[R, E, _]) {
  import zhttp.http._

  def start(port: Int): ZIO[R, Throwable, Nothing] =
    zhttp.service.Server.start(port, toHttpApp)

  private def toHttpApp: HttpApp[R, E] =
    Handlers
      .handlersToList(handlers)
      .map { handler =>
        EndpointParser.handlerToHttpApp(handler.handler)
      }
      .reduce(_ ++ _)
}

object Server {
  def make[R, E <: Throwable, EndpointIds, HandlerIds](
      endpoints: Endpoints[EndpointIds],
      handlers: Handlers[R, E, HandlerIds]
//  )(implicit matches: EndpointIds Matches HandlerIds): Server[R, E] =
  ): Server[R, E] =
    Server(endpoints, handlers)

  def start[R, E <: Throwable, EndpointIds, HandlerIds](
      port: Int,
      endpoints: Endpoints[EndpointIds],
      handlers: Handlers[R, E, HandlerIds]
  ): ZIO[R, Throwable, Nothing] =
//  )(implicit matches: EndpointIds Matches HandlerIds): ZIO[R, Throwable, Nothing] =
    make(endpoints, handlers).start(port)
}
