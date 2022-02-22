package zio.api

import zhttp.http.{Header => HttpHeader, Http, HttpApp, Request, Response}
import zio.json._
import zio.{UIO, ZIO}

private[api] object ServerInterpreter {

  def handlerToHttpApp[R, E, Params, Input, Output](
      handler: HandlerImpl[R, E, Params, Input, Output]
  ): HttpApp[R, E] = {
    val parser: PartialFunction[Request, Params]    = (handler.api.requestParser.parseRequest _).unlift
    implicit val outputEncoder: JsonEncoder[Output] = handler.api.outputCodec.encoder
    implicit val inputDecoder: JsonDecoder[Input]   = handler.api.inputCodec.decoder

    def withInput(request: Request)(process: Input => ZIO[R, E, Response]): ZIO[R, E, Response] =
      if (handler.api.inputCodec == unitCodec) {
        process(().asInstanceOf[Input])
      } else {
        request.bodyAsString
          .flatMap { string =>
            string.fromJson[Input] match {
              case Left(err)    => UIO(Response.text(s"Invalid input: $err"))
              case Right(value) => process(value)
            }
          }
          .catchAll { err =>
            UIO(Response.text(s"Error parsing request body: $err"))
          }
      }

    Http.collectZIO {
      case req @ parser(result) if req.method == handler.api.method.toZioHttpMethod =>
        withInput(req) { input =>
          handler
            .handle((result, input))
            .map { a =>
              if (handler.api.outputCodec == unitCodec) {
                Response.ok
              } else {
                Response.json(a.toJson)
              }
            }
        }
    }
  }

}
