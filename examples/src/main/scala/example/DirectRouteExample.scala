package example

import zhttp.http.{Http, HttpApp}
import zio._
import zio.api._

import java.util.UUID

object DirectRouteExample extends ZIOAppDefault {

  // APIs

  val allTalks =
    API
      .get("talks")
      .query(string("name").?)
      .output[List[Talk]]
      .toHttp {
        case Some(filter) =>
          Talks.all.map(_.filter(_.title.toLowerCase.contains(filter.toLowerCase)))
        case _ =>
          Talks.all
      }

  val getTalk =
    API
      .get("talks" / uuid)
      .output[Option[Talk]]
      .toHttp { uuid =>
        Talks.get(uuid)
      }

  val createTalk =
    API
      .post("talks")
      .input[CreateTalk]
      .output[Talk]
      .toHttp { case CreateTalk(title, description, speaker) =>
        Talks.create(title, description, speaker)
      }

  val deleteTalk =
    API
      .delete("talks" / uuid)
      .toHttp { id =>
        Talks.delete(id)
      }

  val app = getTalk ++ allTalks ++ createTalk ++ deleteTalk

  val program =
    zhttp.service.Server
      .start(8081, app ++ Http.notFound)
      .provide(Talks.live, Logger.live)

  override val run = program

}
