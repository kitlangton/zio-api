package zio.api

import org.openjdk.jmh.annotations._
import zhttp.http.{Headers, Request, URL}

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class RequestParsingBenchmark {
  val request = get("/users/1/comments/10/dogs/hello/cats/nice/mammals/fine/eskimos?name=cool")

  def get(url: String, headers: Map[String, String] = Map.empty): Request =
    Request(url = URL.fromString(url).toOption.get, headers = Headers(headers.toList))

  object ApiParser {
    val api = API
      .get("users" / int / "comments" / int / "dogs" / string / "cats" / string / "mammals" / string / "eskimos")
      .query(string("name"))
    val parse: Request => Option[(Int, Int, String, String, String, String)] = api.requestParser.parseRequest
  }

  // 2/23/2022 1:30 PM RequestParsingBenchmark.parseRequestAPI  thrpt   10  7900694.567 ± 553012.893  ops/s
  // 2/24/2022 2:05 PM RequestParsingBenchmark.parseRequestAPI  thrpt   10  8449007.556 ± 135448.632  ops/s
  @Benchmark
  def parseRequestAPI(): Option[(Int, Int, String, String, String, String)] =
    ApiParser.parse(request)

  object PatternMatchingParser {
    import zhttp.http._
    def parse(request: Request): Option[(Int, Int, String, String, String, String)] =
      request match {
        case req @ Method.GET -> !! / "users" / n1 / "comments" / n2 / "dogs" / dogId / "cats" / catId / "mammals" / mammalId / "eskimos"
            if req.url.queryParams.contains("name") =>
          Some(n1.toInt, n2.toInt, dogId, catId, mammalId, req.url.queryParams("name").head)
      }
  }

//  @Benchmark
//  def parseRequestPatternMatching(): Option[(Int, Int, String, String, String, String)] =
//    PatternMatchingParser.parse(request)

}
