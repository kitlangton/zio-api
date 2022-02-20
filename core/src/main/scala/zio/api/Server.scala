package zio.api

import zio.api.macros.Matches
import zio.ZIO

final case class Server[R, E <: Throwable](apis: APIs[_], handlers: Handlers[R, E, _]) {
  import zhttp.http._

  def start(port: Int): ZIO[R, Throwable, Nothing] =
    zhttp.service.Server.start(port, toHttpApp)

  private def toHttpApp: HttpApp[R, E] =
    Handlers
      .handlersToList(handlers)
      .map { handler =>
        ServerInterpreter.handlerToHttpApp(handler.handler)
      }
      .reduce(_ ++ _)
}

object Server {
  def make[R, E <: Throwable, ApiIds, HandlerIds](
      apis: APIs[ApiIds],
      handlers: Handlers[R, E, HandlerIds]
  )(implicit matches: ApiIds Matches HandlerIds): Server[R, E] =
    Server(apis, handlers)

  def start[R, E <: Throwable, ApiIds, HandlerIds](
      port: Int,
      apis: APIs[ApiIds],
      handlers: Handlers[R, E, HandlerIds]
  )(implicit matches: ApiIds Matches HandlerIds): ZIO[R, Throwable, Nothing] =
    make(apis, handlers).start(port)
}
