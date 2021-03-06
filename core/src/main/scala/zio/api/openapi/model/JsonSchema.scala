package zio.api.openapi.model

import zio.schema._

import zio.json.{DeriveJsonEncoder, JsonEncoder}

sealed trait SchemaObject extends Product with Serializable { self =>
  def withNullable: SchemaObject =
    self match {
      case base: SchemaObject.SchemaObjectBase =>
        base.copy(nullable = Some(true))
      case SchemaObject.AnyOf(objects) =>
        SchemaObject.AnyOf(objects.map(_.withNullable))
    }

}

object SchemaObject {
  final case class SchemaObjectBase(
      `type`: SchemaType,
      items: Option[SchemaObject],
      format: Option[String],
      description: Option[String],
      properties: Option[Map[String, SchemaObject]],
      nullable: Option[Boolean] = None
  ) extends SchemaObject

  object SchemaObjectBase {
    implicit val encoder: JsonEncoder[SchemaObjectBase] =
      DeriveJsonEncoder.gen[SchemaObjectBase]
  }

  final case class AnyOf(objects: List[SchemaObject]) extends SchemaObject

  object AnyOf {
    implicit val encoder: JsonEncoder[AnyOf] =
      JsonEncoder.map[String, List[SchemaObject]].contramap[AnyOf] { oneOf =>
        Map("anyOf" -> oneOf.objects)
      }
  }

  implicit val encoder: JsonEncoder[SchemaObject] =
    JsonEncoder.defer(
      SchemaObjectBase.encoder
        .orElseEither(AnyOf.encoder)
        .contramap[SchemaObject] { //
          case base: SchemaObjectBase =>
            Left(base)
          case oneOf: AnyOf =>
            Right(oneOf)
        }
    )

  def schemaTypeFromStandardType[A](standardType: StandardType[A]): SchemaType =
    standardType match {
      case StandardType.StringType     => SchemaType.String
      case StandardType.BoolType       => SchemaType.Boolean
      case StandardType.ShortType      => SchemaType.Number
      case StandardType.IntType        => SchemaType.Number
      case StandardType.LongType       => SchemaType.Number
      case StandardType.FloatType      => SchemaType.Number
      case StandardType.DoubleType     => SchemaType.Number
      case StandardType.CharType       => SchemaType.String
      case StandardType.UUIDType       => SchemaType.String
      case StandardType.BigDecimalType => SchemaType.Number
      case StandardType.BigIntegerType => SchemaType.Number
      case _                           => throw new Error(s"SCHEMA TYPE NOT SUPPORTED FOR $standardType")
    }

  def formatFromStandardType[A](standardType: StandardType[A]): Option[String] =
    standardType match {
      case StandardType.UUIDType => Some("uuid")
      case _                     => None
    }

  def fromSchema[A](schema: Schema[A]): SchemaObject =
    schema match {
      case enum: Schema.Enum[_] => ???
      case record: Schema.Record[_] =>
        val properties: Map[String, SchemaObject] =
          record.structure.map { case Schema.Field(label, schema, _) =>
            label -> fromSchema(schema)
          }.toMap
        SchemaObjectBase(
          SchemaType.Object,
          None,
          None,
          None,
          Some(properties)
        )
      case collection: Schema.Collection[_, _] =>
        collection match {
          case seq: Schema.Sequence[_, _] =>
            SchemaObjectBase(
              `type` = SchemaType.Array,
              items = Some(fromSchema(seq.schemaA)),
              format = None,
              description = None,
              properties = None
            )
        }

      case Schema.Transform(codec, f, g, annotations) => ???
      case Schema.Primitive(standardType, annotations) =>
        val tpe    = schemaTypeFromStandardType(standardType)
        val format = formatFromStandardType(standardType)
        SchemaObjectBase(
          `type` = tpe,
          items = None,
          format = format,
          description = Some(s"$tpe"),
          properties = None
        )
      case Schema.Optional(codec, annotations) =>
        fromSchema(codec).withNullable
      case Schema.Fail(message, annotations)             => ???
      case Schema.Tuple(left, right, annotations)        => ???
      case Schema.EitherSchema(left, right, annotations) => ???
      case Schema.Lazy(schema0) =>
        fromSchema(schema0())
      case Schema.Meta(ast, annotations) => ???
    }
}

sealed trait SchemaType extends Product with Serializable

object SchemaType {
  case object String  extends SchemaType
  case object Number  extends SchemaType
  case object Integer extends SchemaType
  case object Boolean extends SchemaType
  case object Array   extends SchemaType
  case object Object  extends SchemaType
  case object Null    extends SchemaType

  implicit val encoder: JsonEncoder[SchemaType] =
    JsonEncoder.string.contramap {
      case SchemaType.String  => "string"
      case SchemaType.Number  => "number"
      case SchemaType.Integer => "integer"
      case SchemaType.Boolean => "boolean"
      case SchemaType.Array   => "array"
      case SchemaType.Object  => "object"
      case SchemaType.Null    => "null"
    }
}
