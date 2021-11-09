package zio

import java.util.UUID
import scala.language.implicitConversions

package object route {

  // Routes
  val string: Route[String]   = Route.MatchParser("string", Parser.stringParser)
  val int: Route[Int]         = Route.MatchParser("int", Parser.intParser)
  val boolean: Route[Boolean] = Route.MatchParser("boolean", Parser.booleanParser)
  val uuid: Route[UUID]       = Route.MatchParser("uuid", Parser.uuidParser)

  // Query Params
  def string(name: String): QueryParams[String]   = QueryParams.SingleParam(name, Parser.stringParser)
  def int(name: String): QueryParams[Int]         = QueryParams.SingleParam(name, Parser.intParser)
  def boolean(name: String): QueryParams[Boolean] = QueryParams.SingleParam(name, Parser.booleanParser)

  implicit def string2Route(string: String): Route[Unit] = Route.path(string)

}
