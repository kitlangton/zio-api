package zio.api.openapi

import zio.json._
import zio.json.ast.Json
import zio.api.HttpMethod

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
    path: ApiPath,
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

final case class ApiPath(components: List[PathComponent]) {
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
    deprecated: Boolean = false
//    allowEmptyValue: Boolean = false
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
      path = ApiPath(
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
      path = ApiPath(
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

object OpenApiInterpreter {
  import zio.api._
  import Path._

  def getPath(requestParser: RequestParser[_]): ApiPath =
    ApiPath(getPathComponents(requestParser.getPath))

  def getRouteImpl(requestParser: RequestParser[_]): Option[Path[_]] =
    requestParser match {
      case RequestParser.Zip(left, right) =>
        getRouteImpl(left) orElse getRouteImpl(right)
      case RequestParser.Map(info, _, _) =>
        getRouteImpl(info)
      case _: Header[_] =>
        None
      case _: Query[_] =>
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
      case Path.MapPath(info, _, _) =>
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
      case Path.MapPath(route, _, _) =>
        getPathComponents(route)
    }

  def pathToParameterObjects(route: Path[_]): List[ParameterObject] =
    route match {
      case MatchLiteral(_) => List.empty
      case MatchParser(name, _) =>
        List(ParameterObject(name = name, in = ParameterLocation.Path, required = true))
      case Zip(left, right) =>
        pathToParameterObjects(left) ++ pathToParameterObjects(right)
      case Path.End =>
        List.empty
      case MapPath(route, _, _) =>
        pathToParameterObjects(route)
    }

  def queryParamsToParameterObjects(queryParams: Query[_], optional: Boolean = false): List[ParameterObject] =
    queryParams match {
      case Query.SingleParam(name, _) =>
        List(ParameterObject(name = name, in = ParameterLocation.Query, required = !optional))
      case Query.Zip(left, right) =>
        queryParamsToParameterObjects(left, optional) ++ queryParamsToParameterObjects(right, optional)
      case Query.MapParams(params, _, _) =>
        queryParamsToParameterObjects(params, optional)
      case Query.Optional(params) =>
        queryParamsToParameterObjects(params, true)
    }

  def apiToOperation(api: API[_, _, _]): Map[String, OperationObject] =
    Map(
      api.method.toString.toLowerCase ->
        OperationObject(
          None,
          None,
          pathToParameterObjects(api.requestParser.getPath) ++
            api.requestParser.getQueryParams.toList.flatMap(queryParamsToParameterObjects(_))
        )
    )

  def apiToPaths(apis: List[API[_, _, _]]): Paths =
    Paths(
      apis.map(api =>
        PathObject(
          path = getPath(api.requestParser),
          operations = apiToOperation(api)
        )
      )
    )

  val exampleApi =
    API
      .get("users" / uuid / "posts" / uuid)
      .query(string("name").?)

  val exampleApi2 =
    API
      .post("users")

  def main(args: Array[String]): Unit = {
    val apis = List(exampleApi, exampleApi2)
    println(apiToPaths(apis).toJsonPretty)
  }
}
