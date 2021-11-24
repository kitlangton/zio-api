package zio.route.openapi

import zio.json._
import zio.json.ast.Json
import zio.route.HttpMethod

final case class Paths(
    pathObjects: List[PathObject]
)

object Paths {
  implicit val encoder: JsonEncoder[Paths] =
    JsonEncoder[Json].contramap { (paths: Paths) =>
      val pieces = paths.pathObjects.map { path =>
        path.path.render -> path.operations.toJsonAST.toOption.get
      }
      Json.Obj(pieces: _*)
    }
}

final case class PathObject(
    path: FullPath,
    operations: Map[String, OperationObject]
)

object PathObject {
  implicit val pathJsonEncoder: JsonEncoder[PathObject] =
    JsonEncoder[zio.json.ast.Json].contramap { (path: PathObject) =>
      Json.Obj(
        path.path.render -> path.operations.toJsonAST.toOption.get
      )
    }
}

final case class FullPath(components: List[PathComponent]) {
  def render: String =
    components.map(_.render).mkString("/", "/", "")
}

sealed trait PathComponent extends Product with Serializable {
  def render: String = this match {
    case PathComponent.Literal(string)  => string
    case PathComponent.Variable(string) => s"{$string}"
  }
}

object PathComponent {
  final case class Literal(string: String)  extends PathComponent
  final case class Variable(string: String) extends PathComponent
}

final case class OperationObject(
    summary: Option[String],
    description: Option[String],
    parameters: List[ParameterObject]
)

object OperationObject {
  implicit val operationObjectJsonEncoder: JsonEncoder[OperationObject] =
    DeriveJsonEncoder.gen
}

final case class ParameterObject(
    name: String,
    in: ParameterLocation,
    description: Option[String] = None,
    required: Boolean = false,
    deprecated: Boolean = false,
    allowEmptyValue: Boolean = false
)

object ParameterObject {
  implicit val encoder: JsonEncoder[ParameterObject] =
    DeriveJsonEncoder.gen
}

sealed trait ParameterLocation extends Product with Serializable

object ParameterLocation {
  case object Query  extends ParameterLocation
  case object Header extends ParameterLocation
  case object Path   extends ParameterLocation
  case object Cookie extends ParameterLocation

  implicit val encoder: JsonEncoder[ParameterLocation] =
    JsonEncoder.string.contramap {
      case ParameterLocation.Query  => "query"
      case ParameterLocation.Header => "header"
      case ParameterLocation.Path   => "path"
      case ParameterLocation.Cookie => "cookie"
    }
}

object OpenAPI {
  val example =
    PathObject(
      path = FullPath(
        List(
          PathComponent.Literal("users"),
          PathComponent.Variable("userId")
        )
      ),
      operations = Map(
        HttpMethod.GET.toString.toLowerCase -> OperationObject(
          summary = Some("Get user"),
          description = Some("Get user by id"),
          parameters = List(
            ParameterObject(
              name = "userId",
              in = ParameterLocation.Path,
              required = true
            )
          )
        )
      )
    )

  val postComments =
    PathObject(
      path = FullPath(
        List(
          PathComponent.Literal("posts"),
          PathComponent.Variable("postId"),
          PathComponent.Literal("comments")
        )
      ),
      operations = Map(
        HttpMethod.POST.toString.toLowerCase -> OperationObject(
          summary = Some("Post comment"),
          description = Some("Post comment to post"),
          parameters = List(
            ParameterObject(
              name = "postId",
              in = ParameterLocation.Path,
              required = true
            )
          )
        )
      )
    )

  val paths = Paths(List(example, postComments))

  def main(args: Array[String]): Unit =
    println(paths.toJsonPretty)
}

object EndpointToOpenAPI {
  import zio.route._
  import Path._

  private def getApiPath(requestParser: RequestParser[_]): FullPath =
    FullPath(getPathComponents(getPath(requestParser)))

  private def getPath(requestParser: RequestParser[_]): Path[_] =
    getPathImpl(requestParser).get

  private def getPathImpl(requestParser: RequestParser[_]): Option[Path[_]] =
    requestParser match {
      case RequestParser.Zip(left, right) =>
        getPathImpl(left) orElse getPathImpl(right)
      case RequestParser.Map(info, _) =>
        getPathImpl(info)
      case _: Headers[_] =>
        None
      case _: QueryParams[_] =>
        None
      case route: Path[_] =>
        Some(route)
    }

  def getRightmostLiteral(route: Path[_]): Option[String] =
    route match {
      case Path.MatchLiteral(literal) =>
        Some(literal)
      case Path.Zip(left, right) =>
        getRightmostLiteral(right) orElse getRightmostLiteral(left)
      case Path.MapPath(info, _) =>
        getRightmostLiteral(info)
      case _ => None
    }

  def getPathComponents(route: Path[_], name: Option[String] = None): List[PathComponent] =
    route match {
      case Path.MatchLiteral(string) =>
        List(PathComponent.Literal(string))
      case Path.MatchParser(tpeName, _) =>
        List(PathComponent.Variable(name.map(_ + "Id").getOrElse(tpeName)))
      case Path.Zip(left, right) =>
        getPathComponents(left, name) ++ getPathComponents(right, getRightmostLiteral(left))
      case Path.End =>
        List.empty
      case Path.MapPath(route, _) =>
        getPathComponents(route)
    }

  def routeToParameterObjects(route: Path[_]): List[ParameterObject] =
    route match {
      case MatchLiteral(_) => List.empty
      case MatchParser(name, _) =>
        List(ParameterObject(name = name, in = ParameterLocation.Query, required = true))
      case Zip(left, right) =>
        routeToParameterObjects(left) ++ routeToParameterObjects(right)
      case Path.End =>
        List.empty
      case MapPath(route, _) =>
        routeToParameterObjects(route)
    }

  def endpointToOperation(endpoint: Endpoint[_, _, _]): Map[String, OperationObject] =
    Map(
      endpoint.method.toString.toLowerCase ->
        OperationObject(
          None,
          None,
          routeToParameterObjects(getPath(endpoint.requestParser))
        )
    )

  def endpointToPaths(endpoints: List[Endpoint[_, _, _]]): Paths =
    Paths(
      endpoints.map(endpoint =>
        PathObject(
          path = getApiPath(endpoint.requestParser),
          operations = endpointToOperation(endpoint)
        )
      )
    )

  val exampleEndpoint: Endpoint[_, _, _] =
    Endpoint.get("users" / uuid / "posts" / uuid)

  def main(args: Array[String]): Unit = {
    val endpoints = List(exampleEndpoint)
    println(endpointToPaths(endpoints).toJsonPretty)
  }
}
