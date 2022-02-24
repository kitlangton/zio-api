package example

import zio._

import java.util.UUID

trait Users {
  def allUsers: Task[List[User]]
  def getUser(id: UUID): Task[Option[User]]
  def createUser(name: String, email: String): Task[User]
  def deleteUser(id: UUID): Task[Unit]
}

object Users {
  // accessors
  def allUsers: ZIO[Users, Throwable, List[User]] =
    ZIO.serviceWithZIO[Users](_.allUsers)

  def getUser(id: UUID): ZIO[Users, Throwable, Option[User]] =
    ZIO.serviceWithZIO[Users](_.getUser(id))

  def createUser(name: String, email: String): ZIO[Users, Throwable, User] =
    ZIO.serviceWithZIO[Users](_.createUser(name, email))

  def deleteUser(id: UUID): ZIO[Users, Throwable, Unit] =
    ZIO.serviceWithZIO[Users](_.deleteUser(id))

  final case class UsersLive(log: Logger, ref: Ref[Map[UUID, User]]) extends Users {
    override def allUsers: Task[List[User]] =
      log.log("GETTING ALL USERS") *>
        ref.get.map(_.values.toList)

    override def getUser(id: UUID): Task[Option[User]] =
      log.log(s"GETTING USER $id") *>
        ref.get.map(_.get(id))

    override def createUser(name: String, email: String): Task[User] = {
      val user = User(UUID.randomUUID(), name, email)
      ref.update(_ + (user.id -> user)).as(user)
    }

    override def deleteUser(id: UUID): Task[Unit] =
      ref.update(_ - id)
  }

  val kit: User   = User(UUID.randomUUID(), "Kit Langton", "kit.langton@email.com")
  val olive: User = User(UUID.randomUUID(), "Olive Kitteridge", "olive.kitteridge57@gmail.gov")
  val stacy: User = User(UUID.randomUUID(), "Stacy Smith", "stacymail@stacy.cool")

  var exampleUsers: Map[UUID, User] =
    Map(
      kit.id   -> kit,
      olive.id -> olive,
      stacy.id -> stacy
    )

  val live: ZLayer[Logger, Nothing, Users] = {
    for {
      logger <- ZIO.service[Logger]
      ref    <- Ref.make(exampleUsers)
    } yield UsersLive(logger, ref)
  }.toLayer[Users]
}
