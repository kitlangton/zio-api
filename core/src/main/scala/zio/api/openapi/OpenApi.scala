package zio.api.openapi

import zio.api.HttpMethod
import zio.api.openapi.model._
import zio.json._

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
      case Path.MatchParser(tpeName, _, _) =>
        List(PathComponent.Variable(name.map(_ + "Id").getOrElse(tpeName)))
      case Path.Zip(left, right) =>
        getPathComponents(left, name) ++ getPathComponents(right, getRightmostLiteral(left))
      case Path.End =>
        List.empty
      case Path.MapPath(route, _, _) =>
        getPathComponents(route)
    }

  def pathToParameterObjects(route: Path[_], name: Option[String] = None): List[ParameterObject] =
    route match {
      case MatchLiteral(_) => List.empty
      case MatchParser(matchName, _, schema) =>
        List(
          ParameterObject(
            name = name.map(_ + "Id").getOrElse(matchName),
            in = ParameterLocation.Path,
            required = true,
            schema = SchemaObject.fromSchema(schema)
          )
        )
      case Zip(left, right) =>
        pathToParameterObjects(left, name) ++ pathToParameterObjects(right, getRightmostLiteral(left))
      case Path.End =>
        List.empty
      case MapPath(route, _, _) =>
        pathToParameterObjects(route)
    }

  def queryParamsToParameterObjects(queryParams: Query[_], optional: Boolean = false): List[ParameterObject] =
    queryParams match {
      case Query.SingleParam(name, _, schema) =>
        List(
          ParameterObject(
            name = name,
            in = ParameterLocation.Query,
            required = !optional,
            schema = SchemaObject.fromSchema(schema)
          )
        )
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
            api.requestParser.getQueryParams.toList.flatMap(queryParamsToParameterObjects(_)),
          // TODO: Flesh this out
          Map(
            "200" -> ResponseObject("OK")
          )
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
      .get("users" / int / "posts" / uuid)
      .query(string("name").?)

  val exampleApi2 =
    API
      .post("users")

  def main(args: Array[String]): Unit = {
    val apis = List(exampleApi, exampleApi2)
    println(apiToPaths(apis).toJsonPretty)
  }
}
