package zio.route

import zio.ZIO

final case class Handler[R, E, Params, Input, Output](
    endpoint: Endpoint[Params, Input, Output],
    handle: ((Params, Input)) => ZIO[R, E, Output]
) { self =>
  type Id

  def ++[R1 <: R, E1 >: E](that: Handler[R1, E1, _, _, _]): Handlers[R1, E1, Id with that.Id] =
    Handlers(self) ++ that

}

object Handler {
  type WithId[R, E, Params, Input, Output, Id0] = Handler[R, E, Params, Input, Output] { type Id = Id0 }

  def make[R, E, Params, Input, Output](
      endpoint: Endpoint[Params, Input, Output]
  )(
      handle: ((Params, Input)) => ZIO[R, E, Output]
  ): Handler.WithId[R, E, Params, Input, Output, endpoint.Id] =
    Handler(endpoint, handle).asInstanceOf[Handler.WithId[R, E, Params, Input, Output, endpoint.Id]]
}
