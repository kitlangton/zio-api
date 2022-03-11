package example

import zio._

import java.util.UUID

// A CRUD-dy Talks Service

trait Talks {

  def all: Task[List[Talk]]

  def get(id: UUID): Task[Option[Talk]]

  def create(title: String, description: String, duration: Int): Task[Talk]

  def delete(id: UUID): Task[Unit]

}

object Talks {

  // Accessors
  def all: ZIO[Talks, Throwable, List[Talk]] =
    ZIO.serviceWithZIO[Talks](_.all)

  def get(id: UUID): ZIO[Talks, Throwable, Option[Talk]] =
    ZIO.serviceWithZIO[Talks](_.get(id))

  def create(title: String, description: String, duration: Int): ZIO[Talks, Throwable, Talk] =
    ZIO.serviceWithZIO[Talks](_.create(title, description, duration))

  def delete(id: UUID): ZIO[Talks, Throwable, Unit] =
    ZIO.serviceWithZIO[Talks](_.delete(id))

  final case class TalksLive(log: Logger, ref: Ref[Map[UUID, Talk]]) extends Talks {
    override def all: Task[List[Talk]] =
      log.log("GETTING ALL USERS") *>
        ref.get.map(_.values.toList)

    override def get(id: UUID): Task[Option[Talk]] =
      log.log(s"GETTING USER $id") *>
        ref.get.map(_.get(id))

    override def create(title: String, description: String, duration: Int): Task[Talk] = {
      val talk = Talk(UUID.randomUUID(), title, description, duration)
      ref.update(_ + (talk.id -> talk)).as(talk)
    }

    override def delete(id: UUID): Task[Unit] =
      ref.update(_ - id)
  }

  val kitTalk      = Talk(UUID.randomUUID(), "Intro to ZIO API", "Kit Langton", 10)
  val adamTalk     = Talk(UUID.randomUUID(), "Scopes! Scopes! Scopes!", "Adam Fraser", 10)
  val wiemTalk     = Talk(UUID.randomUUID(), "Book Your Spot and ZIO!", "Wiem Zine Elabidine", 10)
  val exampleTalks = List(kitTalk, adamTalk, wiemTalk).map(t => t.id -> t).toMap

  val live: ZLayer[Logger, Nothing, Talks] = {
    for {
      logger <- ZIO.service[Logger]
      ref    <- Ref.make(exampleTalks)
    } yield TalksLive(logger, ref)
  }.toLayer[Talks]
}
