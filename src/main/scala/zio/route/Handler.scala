package zio.route

import zio.ZIO

final case class Handler[R, E, Params, Input, Output](
    endpoint: Endpoint[Params, Input, Output],
    handle: ((Params, Input)) => ZIO[R, E, Output]
)

object Handler {
  def make[R, E, Params, Input, Output](
      endpoint: Endpoint[Params, Input, Output]
  )(
      handle: ((Params, Input)) => ZIO[R, E, Output]
  ): Handler[R, E, Params, Input, Output] =
    Handler(endpoint, handle)
}
