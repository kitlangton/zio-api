package zio.route

import zhttp.http.{Header, HttpApp, Request, Response}
import zio.{ZIO, Zippable}

sealed trait RequestParser[+A] extends Product with Serializable { self =>
  private[route] def ++[B](that: RequestParser[B])(implicit zippable: Zippable[A, B]): RequestParser[zippable.Out] =
    RequestParser.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }

  def map[B](f: A => B): RequestParser[B] =
    RequestParser.Map(self, f)
}

object RequestParser {
  final case class Zip[A, B](left: RequestParser[A], right: RequestParser[B]) extends RequestParser[(A, B)]

  final case class Map[A, B](info: RequestParser[A], f: A => B) extends RequestParser[B]
}

/** HEADERS
  * =======
  */
sealed trait Headers[+A] extends RequestParser[A] {
  self =>

  def ? : Headers[Option[A]] =
    Headers.Optional(self)

  override def map[B](f: A => B): Headers[B] =
    Headers.Map(self, f)

  def ++[B](that: Headers[B])(implicit zippable: Zippable[A, B]): Headers[zippable.Out] =
    Headers.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }

  private[route] def parse(request: Request): Option[A] =
    self match {
      case Headers.SingleHeader(name, parser) =>
        request.headers.collectFirst { case Header(`name`, value) =>
          parser.parse(value.toString)
        }.flatten

      case zip: Headers.Zip[_, _] =>
        for {
          l <- zip.left.parse(request)
          r <- zip.right.parse(request)
        } yield (l, r).asInstanceOf[A]

      case optional: Headers.Optional[_] =>
        Some(optional.params.parse(request)).asInstanceOf[Option[A]]

      case Headers.Map(headers, f) =>
        headers.parse(request).map(f.asInstanceOf[Any => A])

    }
}

object Headers {

  def AcceptEncoding: Headers[String] = string("Accept-Encoding")
  def UserAgent: Headers[String]      = string("User-Agent")
  def Host: Headers[String]           = string("Host")
  def Accept: Headers[String]         = string("Accept")

  def string(name: String): Headers[String] = SingleHeader(name, Parser.stringParser)

  final case class SingleHeader[A](name: String, parser: Parser[A]) extends Headers[A]

  final case class Zip[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]

  final case class Map[A, B](params: Headers[A], f: A => B) extends Headers[B]

  case class Optional[A](params: Headers[A]) extends Headers[Option[A]]
}

/** QUERY PARAMS
  * ============
  */
sealed trait QueryParams[+A] extends RequestParser[A] { self =>
  def ? : QueryParams[Option[A]] = QueryParams.Optional(self)

  def ++[B](that: QueryParams[B])(implicit zippable: Zippable[A, B]): QueryParams[zippable.Out] =
    QueryParams.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }

  override def map[B](f: A => B): QueryParams[B] =
    QueryParams.Map(self, f)

  private[route] def parse(request: Request): Option[A] =
    self match {
      case QueryParams.SingleParam(name, parser) =>
        request.url.queryParams.get(name).flatMap(_.headOption).flatMap(parser.parse)

      case zip: QueryParams.Zip[_, _] =>
        for {
          l <- zip.left.parse(request)
          r <- zip.right.parse(request)
        } yield (l, r).asInstanceOf[A]

      case optional: QueryParams.Optional[_] =>
        Some(optional.params.parse(request)).asInstanceOf[Option[A]]

      case QueryParams.Map(info, f) =>
        info.parse(request).map(f.asInstanceOf[Any => A])
    }
}

object QueryParams {

  def string(name: String): QueryParams[String]   = SingleParam(name, Parser.stringParser)
  def int(name: String): QueryParams[Int]         = SingleParam(name, Parser.intParser)
  def boolean(name: String): QueryParams[Boolean] = SingleParam(name, Parser.booleanParser)

  final case class SingleParam[A](name: String, parser: Parser[A]) extends QueryParams[A]

  final case class Zip[A, B](left: QueryParams[A], right: QueryParams[B]) extends QueryParams[(A, B)]

  final case class Map[A, B](params: QueryParams[A], f: A => B) extends QueryParams[B]

  case class Optional[A](params: QueryParams[A]) extends QueryParams[Option[A]]
}

object Route {
  final case class MatchLiteral(string: String)               extends Route[Unit]
  final case class MatchParser[A](parser: Parser[A])          extends Route[A]
  final case class Zip[A, B](left: Route[A], right: Route[B]) extends Route[(A, B)]
  final case class Map[A, B](route: Route[A], f: A => B)      extends Route[B]

  def parseInterpret[A](route: Route[A]): HttpApp[Any, Nothing] =
    HttpApp.collectM { case route(result) =>
      ZIO.debug(s"RECEIEVD: " + result).as(Response.text(result.toString))
    }

  def parse[A](input: String, route: Route[A]): Option[A] =
    parseImpl(input.split("/").toList, route).map(_._2)

  def parseImpl[A](input: List[String], route: Route[A]): Option[(List[String], A)] =
    route match {
      case MatchLiteral(string) =>
        if (input.headOption.contains(string))
          Some(input.tail -> ().asInstanceOf[A])
        else
          None

      case MatchParser(parser) =>
        parser
          .parse(input.headOption.getOrElse(""))
          .map(input.tail -> _)

      case zip: Zip[_, _] =>
        for {
          (input0, a) <- parseImpl(input, zip.left)
          (input1, b) <- parseImpl(input0, zip.right)
        } yield (input1, (a, b).asInstanceOf[A])

      case Map(route, f) =>
        parseImpl(input, route)
          .map { case (input, output) =>
            (input, f.asInstanceOf[Any => A](output))
          }
    }

  def path(name: String): Route[Unit] = MatchLiteral(name)
  val string: Route[String]           = MatchParser(Parser.stringParser)
  val int: Route[Int]                 = MatchParser(Parser.intParser)
  val boolean: Route[Boolean]         = MatchParser(Parser.booleanParser)

  // Only defined as RouteAspect
  lazy val contentType: Route[MimeType] = ??? // header()

  // val jsonAspect: RouteAspect[(MimeType, String)] = contentType + authHeader

  def header[A](headerType: HeaderType[A]): Route[A] = ???

  implicit def string2Route(string: String): Route[Unit] = path(string)

}

sealed trait Route[+A] extends RequestParser[A] { self =>

  def ??(doc: Doc): Route[A] = ???

  override def map[B](f: A => B): Route[B] =
    Route.Map(self, f)

  def /[B](that: Route[B])(implicit zippable: Zippable[A, B]): Route[zippable.Out] =
    Route.Zip(this, that).map { case (a, b) => zippable.zip(a, b) }

  def /(string: String): Route[A] =
    Route.Zip(this, string).map(_._1)

  def unapply(string: String): Option[A] =
    Route.parse(string, self)

  def unapply(request: Request): Option[A] =
    Route.parseImpl(request.url.path.toList, self).map(_._2)
}
