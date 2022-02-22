package zio.api

import org.openjdk.jmh.annotations._
import zhttp.http.{Headers, Request, URL}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class RequestParsingBenchmark {
  val request = get("/users/1/comments/10?name=cool")

  def get(url: String, headers: Map[String, String] = Map.empty): Request =
    Request(url = URL.fromString(url).toOption.get, headers = Headers(headers.toList))

  object ApiParser {
    val api                                          = API.get("users" / int / "comments" / int).query(string("name"))
    val parse: Request => Option[(Int, Int, String)] = api.requestParser.parseRequest
  }

  @Benchmark
  def parseRequestAPI(): Option[(Int, Int, String)] =
    ApiParser.parse(request)

  object PatternMatchingParser {
    import zhttp.http._
    def parse(request: Request): Option[(Int, Int, String)] =
      request match {
        case req @ Method.GET -> !! / "users" / n1 / "comments" / n2 if req.url.queryParams.contains("name") =>
          Some(n1.toInt, n2.toInt, req.url.queryParams("name").head)
      }
  }

  @Benchmark
  def parseRequestPatternMatching(): Option[(Int, Int, String)] =
    PatternMatchingParser.parse(request)

}
