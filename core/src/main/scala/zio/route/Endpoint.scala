package zio.route

import zhttp.http.{Headers => _, Header => HttpHeader, Path => _, _}
import zio.json._
import zio.json.internal.{RetractReader, Write}
import zio.route.Endpoint.unitCodec
import zio.route.Handler.WithId
import zio.{UIO, ZIO, route}

final case class Endpoint[Params, Input, Output](
    method: HttpMethod,
    requestParser: RequestParser[Params], // Path / Params / Headers
    doc: Doc,
    inputCodec: JsonCodec[Input],
    outputCodec: JsonCodec[Output]
) { self =>
  type Id

  def query[A](queryParams: Query[A])(implicit
      zippable: Zipper[Params, A]
  ): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ queryParams)

  def header[A](headers: Header[A])(implicit zippable: Zipper[Params, A]): Endpoint[zippable.Out, Input, Output] =
    copy(requestParser = requestParser ++ headers)

  def input[Input2](implicit codec: JsonCodec[Input2]): Endpoint[Params, Input2, Output] =
    copy(inputCodec = codec)

  def output[Output2](implicit codec: JsonCodec[Output2]): Endpoint[Params, Input, Output2] =
    copy(outputCodec = codec)

  def ++(that: Endpoint[_, _, _]): Endpoints[Id with that.Id] =
    Endpoints(self) ++ that
}

object Endpoint {

  type WithId[Params, Input, Output, Id0] = Endpoint[Params, Input, Output] { type Id = Id0 }

  trait NotUnit[A]

  object NotUnit {
    implicit def notUnit[A]: NotUnit[A] = new NotUnit[A] {}

    implicit val notUnitUnit1: NotUnit[Unit] = new NotUnit[Unit] {}
    implicit val notUnitUnit2: NotUnit[Unit] = new NotUnit[Unit] {}
  }

  /** Creates an endpoint for DELETE request at the given route.
    */
  def delete[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.DELETE, route)

  /** Creates an endpoint for a GET request at the given route.
    */
  def get[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.GET, route)

  /** Creates an endpoint for a POST request at the given route.
    */
  def post[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.POST, route)

  /** Creates an endpoint for a PUT request at the given route.
    */
  def put[A](route: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.PUT, route)

  /** Creates an endpoint with the given method and route.
    */
  private def method[Params](method: HttpMethod, route: Path[Params]): Endpoint[Params, Unit, Unit] =
    Endpoint(method, route, Doc.empty, unitCodec, unitCodec)

  lazy implicit val unitCodec: JsonCodec[Unit] = JsonCodec(
    (a: Unit, indent: Option[Int], out: Write) => (),
    (trace: List[JsonDecoder.JsonError], in: RetractReader) => ()
  )
}
