package zio.api

import org.openjdk.jmh.annotations._
import zhttp.http.{Headers, Request, URL}

import java.util.concurrent.TimeUnit
import scala.collection.immutable.Range

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class RequestParsingBenchmark {
  @Param(Array("1000"))
  var size: Int = _

  def createTestArray: Array[Int] = Range.inclusive(1, size).toArray.reverse
  def assertSorted(array: Array[Int]): Unit =
    if (!array.sorted.sameElements(array)) {
      throw new Exception("Array not correctly sorted")
    }

  val api       = API.get("users" / int / "comments" / int).query(string("name"))
  val parser    = ServerInterpreter.parseRequest(api.requestParser) _
  val parserOpt = api.requestParser.parseRequest _
  def get(url: String, headers: Map[String, String] = Map.empty): Request =
    Request(url = URL.fromString(url).toOption.get, headers = Headers(headers.toList))
  val request = get("/users/1/comments/10?name=cool")

//    ORIGINAL
//    [info] RequestParsingBenchmark.parseRequest             1000  thrpt       11308713.096          ops/s
//    [info] RequestParsingBenchmark.parseRequestOptimized    1000  thrpt       10871231.109          ops/s
  @Benchmark
  def parseRequest() =
    parser(request)

//    OPTIMIZED
//    [info] RequestParsingBenchmark.parseRequest             1000  thrpt       13847457.016          ops/s
//    [info] RequestParsingBenchmark.parseRequestOptimized    1000  thrpt       13740228.549          ops/s

  @Benchmark
  def parseRequestOptimized() =
    parserOpt(request)

  //    Define on methods
  //    [info] RequestParsingBenchmark.parseRequest             1000  thrpt       18510411.474          ops/s
  //    [info] RequestParsingBenchmark.parseRequestOptimized    1000  thrpt       19053285.823          ops/s

  //    Define on methods everywhere
  //    [info] RequestParsingBenchmark.parseRequest             1000  thrpt       18510411.474          ops/s
  //    [info] RequestParsingBenchmark.parseRequestOptimized    1000  thrpt       19053285.823          ops/s

  //  [info] RequestParsingBenchmark.parseRequest             1000  thrpt       19421902.861          ops/s
  //  [info] RequestParsingBenchmark.parseRequestOptimized    1000  thrpt       20356910.007          ops/s

  //  @Benchmark
//  def catsBubbleSort() =
//    List.fill(1000)(1).sum
}
