package zio.route

import zio.route.macros.Matches
import zio.{Has, ZIO}

final case class Server[R <: Has[_], E <: Throwable](endpoints: Endpoints[_], handlers: Handlers[R, E, _]) {
  import zhttp.http._

  def run(port: Int): ZIO[R, Throwable, Nothing] =
    zhttp.service.Server.start(port, toHttpApp)

  private def toHttpApp: HttpApp[R, E] =
    Handlers
      .handlersToList(handlers)
      .map { handler =>
        EndpointParser.interpret(handler)
      }
      .reduce(_ +++ _)
}

object Server {
  def make[R <: Has[_], E <: Throwable, EndpointIds, HandlerIds](
      endpoints: Endpoints[EndpointIds],
      handlers: Handlers[R, E, HandlerIds]
  )(implicit matches: EndpointIds Matches HandlerIds): Server[R, E] =
    Server(endpoints, handlers)

}
