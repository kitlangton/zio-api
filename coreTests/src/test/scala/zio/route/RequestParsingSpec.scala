package zio.route

import zhttp.http.{Headers, Request, URL}
import zio.test._

object RequestParsingSpec extends DefaultRunnableSpec {

  def parseRequest[A](endpoint: Endpoint[A, _, _]): Request => Option[A] =
    endpoint.requestParser.parseRequest

  def spec =
    suite("RequestParsingSpec")(
      test("parses basic paths") {
        val endpoint = Endpoint.get("users")
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("/users")).contains(()),
          matcher(get("/posts")).isEmpty
        )
      },
      test("parses chained paths") {
        val endpoint = Endpoint.get("users" / "comments")
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("/users/comments")).contains(()),
          matcher(get("/users")).isEmpty,
          matcher(get("/posts/comments")).isEmpty
        )
      },
      test("parses path arguments") {
        val endpoint = Endpoint.get("users" / int)
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("/users/12")).contains(12),
          matcher(get("/users/105")).contains(105),
          matcher(get("/users/10oops")).isEmpty,
          matcher(get("/users/hello")).isEmpty
        )
      },
      test("parses multiple path arguments") {
        val endpoint = Endpoint.get("users" / int / "posts" / string)
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("/users/12/posts/my-first-post")).contains((12, "my-first-post")),
          matcher(get("/users/12/posts")).isEmpty
        )
      },
      test("parses query parameters") {
        val endpoint = Endpoint.get("users").query(string("name") ++ int("age"))
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("/users?name=Kit&age=28")).contains(("Kit", 28)),
          matcher(get("/users?age=28&name=Kit")).contains(("Kit", 28)),
          matcher(get("/users?name=Kit")).isEmpty,
          matcher(get("/users?age=28")).isEmpty,
          matcher(get("/users")).isEmpty
        )
      },
      test("parses optional query parameters") {
        val endpoint = Endpoint.get("users").query(string("name").? ++ int("age").?)
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("/users?name=Kit&age=28")).contains((Some("Kit"), Some(28))),
          matcher(get("/users?age=28&name=Kit")).contains((Some("Kit"), Some(28))),
          matcher(get("/users?name=Kit")).contains((Some("Kit"), None)),
          matcher(get("/users?age=28")).contains((None, Some(28))),
          matcher(get("/users")).contains((None, None)),
          matcher(get("/posts?name=Kit&age=28")).isEmpty
        )
      },
      test("parses headers") {
        val endpoint = Endpoint.get("users").header(Header.string("Accept") ++ Header.string("Content"))
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("users", Map("Accept" -> "application/json", "Content" -> "text")))
            .contains(("application/json", "text")),
          matcher(get("users", Map("Accept" -> "application/json"))).isEmpty,
          matcher(get("users", Map("Content" -> "text"))).isEmpty,
          matcher(get("users")).isEmpty
        )
      },
      test("parses optional headers") {
        val endpoint = Endpoint.get("users").header(Header.string("Accept").? ++ Header.string("Content").?)
        val matcher  = parseRequest(endpoint)

        assertTrue(
          matcher(get("users", Map("Accept" -> "application/json", "Content" -> "text")))
            .contains((Some("application/json"), Some("text"))),
          matcher(get("users", Map("Accept" -> "application/json"))).contains((Some("application/json"), None)),
          matcher(get("users", Map("Content" -> "text"))).contains((None, Some("text"))),
          matcher(get("users")).contains((None, None)),
          matcher(get("posts", Map("Accept" -> "application/json", "Content" -> "text"))).isEmpty
        )
      }
    )

  def get(url: String, headers: Map[String, String] = Map.empty): Request =
    Request(url = URL.fromString(url).toOption.get, headers = Headers(headers.toList))
}
