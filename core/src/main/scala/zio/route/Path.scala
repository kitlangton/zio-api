package zio.route

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
  private[route] final case class Zip[A, B](left: RequestParser[A], right: RequestParser[B])
      extends RequestParser[(A, B)]

  private[route] final case class Map[A, B](info: RequestParser[A], f: A => B, g: B => A) extends RequestParser[B]
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

  private[route] final case class SingleHeader[A](name: String, parser: Parser[A]) extends Headers[A]

  private[route] final case class Zip[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]

  private[route] final case class Map[A, B](headers: Headers[A], f: A => B, g: B => A) extends Headers[B]

  private[route] case class Optional[A](headers: Headers[A]) extends Headers[Option[A]]

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

  private[route] final case class SingleParam[A](name: String, parser: Parser[A]) extends QueryParams[A]

  private[route] final case class Zip[A, B](left: QueryParams[A], right: QueryParams[B]) extends QueryParams[(A, B)]

  private[route] final case class MapParams[A, B](params: QueryParams[A], f: A => B, g: B => A) extends QueryParams[B]

  private[route] case class Optional[A](params: QueryParams[A]) extends QueryParams[Option[A]]

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

  def path(name: String): Path[Unit] = Path.MatchLiteral(name)

  private[route] final case class MatchLiteral(string: String)                        extends Path[Unit]
  private[route] final case class MatchParser[A](name: String, parser: Parser[A])     extends Path[A]
  private[route] final case class Zip[A, B](left: Path[A], right: Path[B])            extends Path[(A, B)]
  private[route] case object End                                                      extends Path[Unit]
  private[route] final case class MapPath[A, B](route: Path[A], f: A => B, g: B => A) extends Path[B]
}
