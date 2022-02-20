package zio.route

import zhttp.http.{Headers => _, Path => _}
import zio.ZIO
import zio.json._
import zio.json.internal.{RetractReader, Write}

final case class Endpoint[Params, Input, Output](
    method: HttpMethod,
    requestParser: RequestParser[Params], // Path / QueryParams / Headers
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

  /** Creates an endpoint for DELETE request at the given path.
    */
  def delete[A](path: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.DELETE, path)

  /** Creates an endpoint for a GET request at the given path.
    */
  def get[A](path: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.GET, path)

  /** Creates an endpoint for a POST request at the given path.
    */
  def post[A](path: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.POST, path)

  /** Creates an endpoint for a PUT request at the given path.
    */
  def put[A](path: Path[A]): Endpoint[A, Unit, Unit] =
    method(HttpMethod.PUT, path)

  /** Creates an endpoint with the given method and path.
    */
  private def method[Params](method: HttpMethod, path: Path[Params]): Endpoint[Params, Unit, Unit] =
    Endpoint(method, path, Doc.empty, unitCodec, unitCodec)

  lazy implicit val unitCodec: JsonCodec[Unit] = JsonCodec(
    (a: Unit, indent: Option[Int], out: Write) => (),
    (trace: List[JsonDecoder.JsonError], in: RetractReader) => ()
  )
}
