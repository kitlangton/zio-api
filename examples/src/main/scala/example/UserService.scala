package example

import zio.{Has, Ref, Task, ZIO, ZLayer}

import java.util.UUID

object UserService {
  // accessors
  def allUsers: ZIO[Has[UserService], Throwable, List[User]] =
    ZIO.serviceWith[UserService](_.allUsers)

  def getUser(id: UUID): ZIO[Has[UserService], Throwable, Option[User]] =
    ZIO.serviceWith[UserService](_.getUser(id))

  def saveUser(name: String, email: String): ZIO[Has[UserService], Throwable, User] =
    ZIO.serviceWith[UserService](_.saveUser(name, email))

  def deleteUser(id: UUID): ZIO[Has[UserService], Throwable, Unit] =
    ZIO.serviceWith[UserService](_.deleteUser(id))

  final case class UserServiceLive(log: Logger, ref: Ref[Map[UUID, User]]) extends UserService {
    override def allUsers: Task[List[User]] =
      log.log("GETTING ALL USERS") *>
        ref.get.map(_.values.toList)

    override def getUser(id: UUID): Task[Option[User]] =
      log.log(s"GETTING USER $id") *>
        ref.get.map(_.get(id))

    override def saveUser(name: String, email: String): Task[User] = {
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

  val live: ZLayer[Has[Logger], Nothing, Has[UserService]] = {
    for {
      logger <- ZIO.service[Logger]
      ref    <- Ref.make(exampleUsers)
    } yield UserServiceLive(logger, ref)
  }.toLayer[UserService]
}

trait UserService {
  def allUsers: Task[List[User]]
  def getUser(id: UUID): Task[Option[User]]
  def saveUser(name: String, email: String): Task[User]
  def deleteUser(id: UUID): Task[Unit]
}
