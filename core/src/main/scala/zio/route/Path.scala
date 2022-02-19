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
        case _: Header[_] =>
          None
        case _: Query[_] =>
          None
        case route: Path[_] =>
          Some(route)
      }

    getPathImpl(self).getOrElse(Path.End)
  }

  private[route] def getQueryParams: Option[Query[_]] =
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
      case route: Query[_] =>
        Some(route)
      case _: Header[_] =>
        None
      case _: Path[_] =>
        None
    }

  private[route] def getHeaders: Option[Header[_]] =
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
      case route: Header[_] =>
        Some(route)
      case _: Query[_] =>
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
sealed trait Header[A] extends RequestParser[A] {
  self =>

  def ? : Header[Option[A]] =
    Header.Optional(self)

  override def map[B](f: A => B)(g: B => A): Header[B] =
    Header.Map(self, f, g)

  def ++[B](that: Header[B])(implicit zippable: Zipper[A, B]): Header[zippable.Out] =
    Header.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }(zippable.unzip)

}

object Header {
  def AcceptEncoding: Header[String] = string("Accept-Encoding")
  def UserAgent: Header[String]      = string("User-Agent")
  def Host: Header[String]           = string("Host")
  def Accept: Header[String]         = string("Accept")

  def string(name: String): Header[String] = SingleHeader(name, Parser.stringParser)

  private[route] final case class SingleHeader[A](name: String, parser: Parser[A]) extends Header[A]

  private[route] final case class Zip[A, B](left: Header[A], right: Header[B]) extends Header[(A, B)]

  private[route] final case class Map[A, B](headers: Header[A], f: A => B, g: B => A) extends Header[B]

  private[route] case class Optional[A](headers: Header[A]) extends Header[Option[A]]

}

/** QUERY PARAMS
  * ============
  */
sealed trait Query[A] extends RequestParser[A] { self =>
  def ? : Query[Option[A]] = Query.Optional(self)

  def ++[B](that: Query[B])(implicit zippable: Zipper[A, B]): Query[zippable.Out] =
    Query.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }(zippable.unzip)

  override def map[B](f: A => B)(g: B => A): Query[B] =
    Query.MapParams(self, f, g)

}

object Query {

  private[route] final case class SingleParam[A](name: String, parser: Parser[A]) extends Query[A]

  private[route] final case class Zip[A, B](left: Query[A], right: Query[B]) extends Query[(A, B)]

  private[route] final case class MapParams[A, B](params: Query[A], f: A => B, g: B => A) extends Query[B]

  private[route] case class Optional[A](params: Query[A]) extends Query[Option[A]]

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
