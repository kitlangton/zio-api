package example

import zio._

trait Logger {
  def log(msg: String): UIO[Unit]
}

object Logger {
  // accessors
  def log(msg: String): ZIO[Has[Logger], Nothing, Unit] =
    ZIO.serviceWith[Logger](_.log(msg))

  // node of your dependency graph
  val live: ULayer[Has[Logger]] =
    UIO(LoggerLive()).toLayer[Logger]

  final case class LoggerLive() extends Logger {
    override def log(msg: String): UIO[Unit] = UIO(println(msg))
  }
}
