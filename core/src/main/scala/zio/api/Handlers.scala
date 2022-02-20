package zio.api

import scala.language.implicitConversions

sealed trait Handlers[-R, +E, Ids] { self =>
  def ++[R1 <: R, E1 >: E](that: Handler[R1, E1]): Handlers[R1, E1, Ids with that.Id] =
    Handlers.Concat[R1, E1, Ids, that.Id](self, Handlers.Single[R1, E1, that.Id](that))

}

object Handlers {

  def handlersToList[R, E, _](handlers: Handlers[R, E, _]): List[Handler[R, E]] =
    handlers match {
      case Concat(l, r) => handlersToList(l) ++ handlersToList(r)
      case Single(h)    => List(h.asInstanceOf[Handler[R, E]])
    }

  def apply[R, E](handler: Handler[R, E]): Handlers[R, E, handler.Id] =
    Single[R, E, handler.Id](handler)

  implicit def handlerToHandlers[R, E](handler: Handler[R, E]): Handlers[R, E, handler.Id] =
    Single[R, E, handler.Id](handler)

  final case class Single[R, E, Id](handler: Handler[R, E]) extends Handlers[R, E, Id]

  final case class Concat[R, E, A, B](left: Handlers[R, E, A], right: Handlers[R, E, B])
      extends Handlers[R, E, A with B]
}
