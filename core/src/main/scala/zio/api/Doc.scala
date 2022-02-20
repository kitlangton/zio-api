package zio.api

sealed trait Doc

object Doc {
  val empty: Doc = Empty

  case object Empty extends Doc
}
