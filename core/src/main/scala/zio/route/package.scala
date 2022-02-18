package zio

import java.util.UUID
import scala.language.implicitConversions

package object route {

  // Routes
  val string: Path[String]   = Path.MatchParser("string", Parser.stringParser)
  val int: Path[Int]         = Path.MatchParser("int", Parser.intParser)
  val boolean: Path[Boolean] = Path.MatchParser("boolean", Parser.booleanParser)
  val uuid: Path[UUID]       = Path.MatchParser("uuid", Parser.uuidParser)

  // Query Params
  def string(name: String): QueryParams[String]   = QueryParams.SingleParam(name, Parser.stringParser)
  def int(name: String): QueryParams[Int]         = QueryParams.SingleParam(name, Parser.intParser)
  def boolean(name: String): QueryParams[Boolean] = QueryParams.SingleParam(name, Parser.booleanParser)

  implicit def string2Route(string: String): Path[Unit] = Path.path(string)

}
