package zio.route

import zhttp.http.Header
import zio.Zippable

import scala.language.implicitConversions

/** A RequestParser is a description of a Path, Query Parameters, and Headers.:
  *   - Path: /users/:id/posts
  *   - Query Parameters: ?page=1&limit=10
  *   - Headers: X-User-Id: 1 or Accept: application/json
  */
sealed trait RequestParser[A] extends Product with Serializable { self =>
  private[route] def ++[B](that: RequestParser[B])(implicit zippable: Zipper[A, B]): RequestParser[zippable.Out] =
    RequestParser.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }(zippable.unzip)

  def map[B](f: A => B)(g: B => A): RequestParser[B] =
    RequestParser.Map(self, f, g)

  private[route] def getPath: Path[_] = {
    def getPathImpl(requestParser: RequestParser[_]): Option[Path[_]] =
      requestParser match {
        case RequestParser.Zip(left, right) =>
          getPathImpl(left) orElse getPathImpl(right)
        case RequestParser.Map(info, _, _) =>
          getPathImpl(info)
        case _: Headers[_] =>
          None
        case _: QueryParams[_] =>
          None
        case route: Path[_] =>
          Some(route)
      }

    getPathImpl(self).getOrElse(Path.End)
  }

  private[route] def getQueryParams: Option[QueryParams[_]] =
    self match {
      case zip: RequestParser.Zip[_, _] =>
        (zip.left.getQueryParams, zip.right.getQueryParams) match {
          case (Some(left), Some(right)) => Some(left ++ right)
          case (Some(left), None)        => Some(left)
          case (None, Some(right))       => Some(right)
          case (None, None)              => None
        }
      case RequestParser.Map(info, _, _) =>
        info.getQueryParams
      case route: QueryParams[_] =>
        Some(route)
      case _: Headers[_] =>
        None
      case _: Path[_] =>
        None
    }

  private[route] def getHeaders: Option[Headers[_]] =
    self match {
      case zip: RequestParser.Zip[_, _] =>
        (zip.left.getHeaders, zip.right.getHeaders) match {
          case (Some(left), Some(right)) => Some(left ++ right)
          case (Some(left), None)        => Some(left)
          case (None, Some(right))       => Some(right)
          case (None, None)              => None
        }
      case RequestParser.Map(info, _, _) =>
        info.getHeaders
      case route: Headers[_] =>
        Some(route)
      case _: QueryParams[_] =>
        None
      case _: Path[_] =>
        None
    }

}

object RequestParser {
  final case class Zip[A, B](left: RequestParser[A], right: RequestParser[B]) extends RequestParser[(A, B)]

  final case class Map[A, B](info: RequestParser[A], f: A => B, g: B => A) extends RequestParser[B]
}

/** =HEADERS=
  */
sealed trait Headers[A] extends RequestParser[A] {
  self =>

  def ? : Headers[Option[A]] =
    Headers.Optional(self)

  override def map[B](f: A => B)(g: B => A): Headers[B] =
    Headers.Map(self, f, g)

  def ++[B](that: Headers[B])(implicit zippable: Zipper[A, B]): Headers[zippable.Out] =
    Headers.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }(zippable.unzip)

}

object Headers {

  def AcceptEncoding: Headers[String] = string("Accept-Encoding")
  def UserAgent: Headers[String]      = string("User-Agent")
  def Host: Headers[String]           = string("Host")
  def Accept: Headers[String]         = string("Accept")

  def string(name: String): Headers[String] = SingleHeader(name, Parser.stringParser)

  final case class SingleHeader[A](name: String, parser: Parser[A]) extends Headers[A]

  final case class Zip[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]

  final case class Map[A, B](headers: Headers[A], f: A => B, g: B => A) extends Headers[B]

  case class Optional[A](headers: Headers[A]) extends Headers[Option[A]]

  private[route] def parse[A](headers: Headers[A], requestHeaders: List[Header]): Option[A] =
    headers match {
      case SingleHeader(name, parser) =>
        requestHeaders.collectFirst { case (`name`, value) =>
          parser.parse(value.toString)
        }.flatten

      case Zip(left, right) =>
        for {
          left  <- parse(left, requestHeaders)
          right <- parse(right, requestHeaders)
        } yield (left, right)

      case Optional(headers) =>
        Some(parse(headers, requestHeaders))

      case Map(headers, f, _) =>
        parse(headers, requestHeaders).map(f.asInstanceOf[Any => A])
    }
}

/** QUERY PARAMS
  * ============
  */
sealed trait QueryParams[A] extends RequestParser[A] { self =>
  def ? : QueryParams[Option[A]] = QueryParams.Optional(self)

  def ++[B](that: QueryParams[B])(implicit zippable: Zipper[A, B]): QueryParams[zippable.Out] =
    QueryParams.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }(zippable.unzip)

  override def map[B](f: A => B)(g: B => A): QueryParams[B] =
    QueryParams.MapParams(self, f, g)

}

object QueryParams {

  final case class SingleParam[A](name: String, parser: Parser[A]) extends QueryParams[A]

  final case class Zip[A, B](left: QueryParams[A], right: QueryParams[B]) extends QueryParams[(A, B)]

  final case class MapParams[A, B](params: QueryParams[A], f: A => B, g: B => A) extends QueryParams[B]

  case class Optional[A](params: QueryParams[A]) extends QueryParams[Option[A]]

  private[route] def parse[A](queryParams: QueryParams[A], requestParams: Map[String, List[String]]): Option[A] =
    queryParams match {
      case SingleParam(name, parser) =>
        requestParams.get(name).flatMap(_.headOption).flatMap(parser.parse)

      case Zip(left, right) =>
        for {
          l <- parse(left, requestParams)
          r <- parse(right, requestParams)
        } yield (l, r).asInstanceOf[A]

      case Optional(params) =>
        Some(parse(params, requestParams)).asInstanceOf[Option[A]]

      case MapParams(info, f, _) =>
        parse(info, requestParams).map(f.asInstanceOf[Any => A])
    }
}

/** A DSL for describe Paths
  *   - ex: /users
  *   - ex: /users/:id/friends
  *   - ex: /users/:id/friends/:friendId
  *   - ex: /posts/:id/comments/:commentId
  */
sealed trait Path[A] extends RequestParser[A] { self =>
  def ??(doc: Doc): Path[A] = ???

  override def map[B](f: A => B)(g: B => A): Path[B] =
    Path.MapPath(self, f, g)

  def /[B](that: Path[B])(implicit zippable: Zipper[A, B]): Path[zippable.Out] =
    Path.Zip(this, that).map { case (a, b) => zippable.zip(a, b) }(zippable.unzip)

  def /(string: String): Path[A] =
    Path.Zip(this, Path.path(string)).map(_._1)(a => (a, ()))
}

object Path {
  // ex: "/users"
  // "users"
  // MatchLiteral("users")
  final case class MatchLiteral(string: String) extends Path[Unit]

  // ex: "/:id" (which must be an int)
  // int
  // MatchParser(Parser.intParser)
  final case class MatchParser[A](name: String, parser: Parser[A]) extends Path[A]

  // ex: "/users/:id"
  // "users" / int
  // Zip(MatchLiteral("users"), MatchParser(Parser.intParser))
  final case class Zip[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]

  case object End extends Path[Unit]

  // case class Person(name: String, age: Int)
  // ("name" / string / "age" / int).map { case (name, age) => Person(name, age) }
  // run(route)(input) : Option[A]
  // "name/kit/age/23" -> Some(Person("kit", 23))
  // "name/kit/age/oops" -> None
  final case class MapPath[A, B](route: Path[A], f: A => B, g: B => A) extends Path[B]

  private[route] def parse[A](route: Path[A], input: List[String]): Option[A] =
    parseImpl(route, input).map(_._2)

  private[route] def parseImpl[A](route: Path[A], input: List[String]): Option[(List[String], A)] =
    route match {
      case MatchLiteral(string) =>
        if (input.headOption.contains(string))
          Some(input.tail -> ())
        else
          None

      case MatchParser(_, parser) =>
        input.headOption.flatMap { head =>
          parser
            .parse(head)
            .map(input.tail -> _)
        }

      case Zip(left, right) =>
        for {
          (input0, a) <- parseImpl(left, input)
          (input1, b) <- parseImpl(right, input0)
        } yield (input1, (a, b))

      case MapPath(route, f, _) =>
        parseImpl(route, input)
          .map { case (input, output) =>
            (input, f(output))
          }

      case End =>
        if (input.isEmpty) Some(input -> ())
        else None

    }

  def path(name: String): Path[Unit] = Path.MatchLiteral(name)

}
