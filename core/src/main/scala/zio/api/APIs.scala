package zio.api

import scala.language.implicitConversions

sealed trait APIs[Ids] {
  def ++(that: API[_, _, _]): APIs[Ids with that.Id] =
    APIs.Concat[Ids, that.Id](this, APIs.Single[that.Id](that))
}

object APIs {
  def apply(api: API[_, _, _]): APIs[api.Id] =
    Single[api.Id](api)

  implicit def api2Apis(api: API[_, _, _]): APIs[api.Id] =
    Single[api.Id](api)

  final case class Single[Id](api: API[_, _, _]) extends APIs[Id]

  final case class Concat[A, B](left: APIs[A], right: APIs[B]) extends APIs[A with B]
}
