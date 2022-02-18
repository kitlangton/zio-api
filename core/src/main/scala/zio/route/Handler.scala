package zio.route

import zhttp.http.HttpApp
import zio.ZIO

case class Handler[-R, +E](handler: HandlerImpl[R, E, _, _, _]) {
  type Id

  def ++[R1 <: R, E1 >: E](that: Handler[R1, E1]): Handlers[R1, E1, Id with that.Id] =
    Handlers(this) ++ that

  def toHttp: HttpApp[R, E] =
    EndpointParser.handlerToHttpApp(handler)
}

final case class HandlerImpl[-R, +E, Params, Input, Output](
    endpoint: Endpoint[Params, Input, Output],
    handle: ((Params, Input)) => ZIO[R, E, Output]
)

object Handler {
  type WithId[R, E, Id0] = Handler[R, E] { type Id = Id0 }

  def make[R, E, Params, Input, Output](
      endpoint: Endpoint[Params, Input, Output]
  )(
      handle: ((Params, Input)) => ZIO[R, E, Output]
  ): Handler.WithId[R, E, endpoint.Id] =
    Handler(HandlerImpl(endpoint, handle)).asInstanceOf[Handler.WithId[R, E, endpoint.Id]]
}
