package example

import zio.json._
import zio.schema._

import java.util.UUID

object Talk {
  implicit val codec: JsonCodec[Talk] = DeriveJsonCodec.gen[Talk]
  implicit val schema: Schema[Talk]   = DeriveSchema.gen
}

final case class Talk(
    id: UUID,
    title: String,
    speaker: String,
    duration: Int
)
