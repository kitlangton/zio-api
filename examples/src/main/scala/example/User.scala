package example

import zio.json._

import java.util.UUID

object User {
  implicit val codec: JsonCodec[User] = DeriveJsonCodec.gen[User]
}

final case class User(id: UUID, name: String, email: String)
