package zio.api

import zhttp.http.{Request, Header => ZHeader}
import zio.schema.Schema

import scala.language.implicitConversions

/** A RequestParser is a description of a Path, Query Parameters, and Headers.:
  *   - Path: /users/:id/posts
  *   - Query Parameters: ?page=1&limit=10
  *   - Headers: X-User-Id: 1 or Accept: application/json
  */
sealed trait RequestParser[A] extends Product with Serializable { self =>
  private[api] def ++[B](that: RequestParser[B])(implicit zippable: Zipper[A, B]): RequestParser[zippable.Out] =
    RequestParser.Zip(self, that).map { case (a, b) => zippable.zip(a, b) }(zippable.unzip)

  def map[B](f: A => B)(g: B => A): RequestParser[B] =
    RequestParser.Map(self, f, g)

  private[api] def getPath: Path[_] = {
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

  private[api] def getQueryParams: Option[Query[_]] =
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

  private[api] def getHeaders: Option[Header[_]] =
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

  private[api] def parseRequest(request: Request): Option[A] = Option(parseRequestImpl(request))

  private[api] def parseRequestImpl(request: Request): A
}

object RequestParser {
  private[api] final case class Zip[A, B](left: RequestParser[A], right: RequestParser[B])
      extends RequestParser[(A, B)] {

    override private[api] def parseRequestImpl(request: Request): (A, B) = {
      val a = left.parseRequestImpl(request)
      if (a == null) return null
      val b = right.parseRequestImpl(request)
      if (b == null) return null
      (a, b)
    }
  }

  private[api] final case class Map[A, B](info: RequestParser[A], f: A => B, g: B => A) extends RequestParser[B] {
    override private[api] def parseRequestImpl(request: Request): B = {
      val a = info.parseRequestImpl(request)
      if (a == null) return null.asInstanceOf[B]
      f(a)
    }
  }
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

  override private[api] def parseRequestImpl(request: Request) = {
    val map: Map[String, String] = request.headers.toChunk.toMap.map { case (k, v) => k.toString -> v.toString }
    parseHeaders(map)
  }

  private[api] def parseHeaders(requestHeaders: Map[String, String]): A

}

object Header {
  def AcceptEncoding: Header[String] = string("Accept-Encoding")
  def UserAgent: Header[String]      = string("User-Agent")
  def Host: Header[String]           = string("Host")
  def Accept: Header[String]         = string("Accept")

  def string(name: String): Header[String] = SingleHeader(name, Parser.stringParser)

  private[api] final case class SingleHeader[A](name: String, parser: Parser[A]) extends Header[A] {
    override private[api] def parseHeaders(requestHeaders: Predef.Map[String, String]): A = {
      val a = requestHeaders.getOrElse(name, null)
      if (a == null) return null.asInstanceOf[A]
      val parsed = parser.parse(a)
      if (parsed.isEmpty) null.asInstanceOf[A]
      else parsed.get
    }
  }

  private[api] final case class Zip[A, B](left: Header[A], right: Header[B]) extends Header[(A, B)] {
    override private[api] def parseHeaders(requestHeaders: Predef.Map[String, String]): (A, B) = {
      val a = left.parseHeaders(requestHeaders)
      if (a == null) return null
      val b = right.parseHeaders(requestHeaders)
      if (b == null) return null
      (a, b)
    }
  }

  private[api] final case class Map[A, B](headers: Header[A], f: A => B, g: B => A) extends Header[B] {
    override private[api] def parseHeaders(requestHeaders: Predef.Map[String, String]): B = {
      val a = headers.parseHeaders(requestHeaders)
      if (a == null) return null.asInstanceOf[B]
      f(a)
    }

  }

  private[api] case class Optional[A](headers: Header[A]) extends Header[Option[A]] {
    override private[api] def parseHeaders(requestHeaders: Predef.Map[String, String]): Option[A] =
      Option(headers.parseHeaders(requestHeaders))

  }

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

  override private[api] def parseRequestImpl(request: Request) =
    parseQueryImpl(request.url.queryParams)

  def parseQueryImpl(params: Map[String, List[String]]): A
}

object Query {

  private[api] final case class SingleParam[A](name: String, parser: Parser[A], schema: Schema[A]) extends Query[A] {
    override def parseQueryImpl(params: Map[String, List[String]]): A = {
      val a = params.getOrElse(name, null)
      if (a == null) return null.asInstanceOf[A]
      val parsed = parser.parse(a.head)
      if (parsed.isEmpty) null.asInstanceOf[A]
      else parsed.get
    }

  }

  private[api] final case class Zip[A, B](left: Query[A], right: Query[B]) extends Query[(A, B)] {
    override def parseQueryImpl(params: Map[String, List[String]]): (A, B) = {
      val a = left.parseQueryImpl(params)
      if (a == null) return null
      val b = right.parseQueryImpl(params)
      if (b == null) return null
      (a, b)
    }

  }

  private[api] final case class MapParams[A, B](params: Query[A], f: A => B, g: B => A) extends Query[B] {
    override def parseQueryImpl(paramsMap: Map[String, List[String]]): B = {
      val a = params.parseQueryImpl(paramsMap)
      if (a == null) return null.asInstanceOf[B]
      f(a)
    }

  }

  private[api] case class Optional[A](params: Query[A]) extends Query[Option[A]] {
    override def parseQueryImpl(paramsMap: Map[String, List[String]]): Option[A] =
      Option(params.parseQueryImpl(paramsMap))
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

  override private[api] def parseRequestImpl(request: Request): A = {
    val a = parseImpl(request.url.path.toList)
    if (a == null) return null.asInstanceOf[A]
    a._2
  }

  private[api] def parseImpl(input: List[String]): (List[String], A)
}

object Path {
  def path(name: String): Path[Unit] = Path.MatchLiteral(name)

  private[api] final case class MatchLiteral(string: String) extends Path[Unit] {
    override private[api] def parseImpl(input: List[String]) =
      if (input.isEmpty) null
      else if (input.head == string) (input.tail, ())
      else null
  }

  private[api] final case class MatchParser[A](name: String, parser: Parser[A], schema: Schema[A]) extends Path[A] {
    override private[api] def parseImpl(input: List[String]): (List[String], A) =
      if (input.isEmpty) null
      else {
        val a = parser.parse(input.head)
        if (a.isEmpty) null
        else (input.tail, a.get)
      }
  }

  private[api] final case class Zip[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)] {
    override private[api] def parseImpl(input: List[String]): (List[String], (A, B)) =
      if (input.isEmpty) null
      else {
        val a = left.parseImpl(input)
        if (a eq null) return null
        val b = right.parseImpl(a._1)
        if (b eq null) return null
        (b._1, (a._2, b._2))
      }
  }

  private[api] case object End extends Path[Unit] {
    override private[api] def parseImpl(input: List[String]) =
      if (input.isEmpty) (input, ())
      else null
  }

  private[api] final case class MapPath[A, B](route: Path[A], f: A => B, g: B => A) extends Path[B] {
    override private[api] def parseImpl(input: List[String]): (List[String], B) =
      if (input.isEmpty) null
      else {
        val a = route.parseImpl(input)
        if (a eq null) return null
        (a._1, f(a._2))
      }
  }
}
