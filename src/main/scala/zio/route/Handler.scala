package zio.route

import zio.ZIO

final case class Handler[R, E, Params, Input, Output](
    endpoint: Endpoint[Params, Input, Output],
    handler: ((Params, Input)) => ZIO[R, E, Output]
)

object Handler {
  def make[R, E, Params, Input, Output](
      endpoint: Endpoint[Params, Input, Output]
  )(
      f: ((Params, Input)) => ZIO[R, E, Output]
  ): Handler[R, E, Params, Input, Output] =
    Handler(endpoint, f)
}
