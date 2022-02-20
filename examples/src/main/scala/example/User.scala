package example

import zio.json._
import zio.schema.{DeriveSchema, Schema}

import java.util.UUID

object User {
  implicit val codec: JsonCodec[User] = DeriveJsonCodec.gen[User]
  implicit val schema: Schema[User]   = DeriveSchema.gen
}

final case class User(id: UUID, name: String, email: String)
