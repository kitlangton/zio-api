package zio.route

import scala.language.implicitConversions

sealed trait Endpoints[Ids] {
  def ++(that: Endpoint[_, _, _]): Endpoints[Ids with that.Id] =
    Endpoints.Concat[Ids, that.Id](this, Endpoints.Single[that.Id](that))
}

object Endpoints {
  def apply(endpoint: Endpoint[_, _, _]): Endpoints[endpoint.Id] =
    Single[endpoint.Id](endpoint)

  implicit def endpointToEndpoints(endpoint: Endpoint[_, _, _]): Endpoints[endpoint.Id] =
    Single[endpoint.Id](endpoint)

  final case class Single[Id](endpoint: Endpoint[_, _, _]) extends Endpoints[Id]

  final case class Concat[A, B](left: Endpoints[A], right: Endpoints[B]) extends Endpoints[A with B]
}
