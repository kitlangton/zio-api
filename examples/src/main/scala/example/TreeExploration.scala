package example

import example.TreeExploration.Opt.addCompletion
import zio.Zippable
import zio.api.Parser

import java.util.UUID
import scala.language.implicitConversions

/** TODO:
  *   - Add `Map` node
  *   - Integrate with actual Path type
  *   - Benchmark
  */
object TreeExploration {

  object Path {
    final case class Literal(value: String)                     extends Path[Unit]
    final case class Match[A](parser: Parser[A])                extends Path[A]
    final case class Zip[A, B](lhs: Path[A], rhs: Path[B])      extends Path[(A, B)]
    final case class ZipRight[A, B](lhs: Path[A], rhs: Path[B]) extends Path[B]

    implicit def path(string: String): Path[Unit] = Literal(string)

    val int: Match[Int]       = Match[Int](Parser[Int])
    val string: Match[String] = Match[String](Parser[String])
    val uuid: Match[UUID]     = Match[UUID](Parser[UUID])
  }

  sealed trait Path[A] extends Product with Serializable {
    def /[B](that: Path[B])(implicit zippable: Zippable[A, B]): Path[zippable.Out] =
      Path.Zip(this, that).asInstanceOf[Path[zippable.Out]]

  }

  final case class State(
      path: Array[String],
      var index: Int,
      var parsed: Int,
      builder: Array[Any],
      length: Int
  ) {

    def outOfBounds: Boolean = index >= length

  }

  sealed trait Opt extends Product with Serializable { self =>

    def ===(that: Opt): Boolean =
      (self, that) match {
        case (Opt.Complete(l), Opt.Complete(r)) => l === r
        case (Opt.Complete(l), r)               => l === r
        case (l, Opt.Complete(r))               => l === r
        case _                                  => self == that
      }

    def run(input: List[String]): Any =
      runImpl(State(input.toArray, 0, 0, Array.ofDim(input.size), input.length))

    def rightAssociate: Opt =
      self match {
        case Opt.Zip(Opt.Zip(a, b), c) => Opt.Zip(a, Opt.Zip(b, c)).rightAssociate
        case other                     => other
      }

    def |(that: Opt): Opt =
      (self, that) match {
        case (Opt.Zip(l1, r1), Opt.Zip(l2, r2)) if l1 === l2 =>
          Opt.Zip(l1, r1 | r2)
        case (l1, Opt.Zip(l2, r2)) if l1 === l2 =>
          Opt.Zip(l1, r2)
        case (Opt.Zip(l1, r1), l2) if l1 === l2 =>
          Opt.Zip(l1, r1 | l2)
        case (Opt.OrElse(l1, r1), that) =>
          if ((r1 | that) == Opt.OrElse(r1, that)) r1 | (l1 | that)
          else l1 | (r1 | that)
        case _ =>
          Opt.OrElse(self, that)
      }

    def render: String =
      self match {
        case Opt.Literal(index, string) =>
          s"Literal($index, $string)"
        case Opt.Parse(index, parser) =>
          s"Parse($index, $parser)"
        case Opt.Zip(left, right) =>
          s"""
Zip(
${left.render.split("\n").map("  " + _).mkString("\n")},
${right.render.split("\n").map("  " + _).mkString("\n")}
)
             """.trim
        case Opt.OrElse(left, right) =>
          s"""
OrElse(
${left.render.split("\n").map("  " + _).mkString("\n")},
${right.render.split("\n").map("  " + _).mkString("\n")}
)
          """.trim
        case Opt.Complete(opt) =>
          s"Opt.Complete($opt)"
      }

    protected def runImpl(state: State): Any
  }

  object Opt {
    final case class Literal(index: Int, string: String) extends Opt {
      override protected def runImpl(state: State): Any =
        if (state.outOfBounds) {
          null
        } else if (state.path(state.index) == string) {
          state.index += 1
        } else {
          null
        }

    }

    final case class Parse(index: Int, parser: Parser[_]) extends Opt {
      override protected def runImpl(state: State): Any =
        if (state.outOfBounds) {
          null
        } else {
          val parsedOption = parser.parse(state.path(state.index))
          if (parsedOption.isDefined) {
            state.builder(index) = parsedOption.get
            state.index += 1
            state.parsed += 1
          } else {
            null
          }
        }
    }

    final case class Zip(left: Opt, right: Opt) extends Opt {
      override protected def runImpl(state: State): Any = {
        val result = left.runImpl(state)
        if (result != null && left.isInstanceOf[Complete]) result
        else right.runImpl(state)
      }
    }

    final case class OrElse(left: Opt, right: Opt) extends Opt {
      override protected def runImpl(state: State): Any = {
        val index  = state.index
        val parsed = state.parsed
        val result = left.runImpl(state)
        if (result != null) return result
        state.index = index
        state.parsed = parsed
        right.runImpl(state)
      }
    }

    final case class Complete(opt: Opt) extends Opt {

      override protected def runImpl(state: State): Any = {
        opt.runImpl(state)
        if (state.index != state.length) return null
        val builder = state.builder
        val result = state.parsed match {
          case 0 => ()
          case 1 => builder(0)
          case 2 => (builder(0), builder(1))
          case 3 => (builder(0), builder(1), builder(2))
          case 4 => (builder(0), builder(1), builder(2), builder(3))
          case 5 => (builder(0), builder(1), builder(2), builder(3), builder(4))
        }

        println(s"RETURNING RESULT! $result")
        result
      }
    }

    def fromPath[A](path: Path[A], index: Int): (Opt, Int) =
      path match {
        case Path.Literal(value) =>
          Literal(index, value) -> index
        case Path.Match(parser) =>
          Parse(index, parser) -> (index + 1)
        case Path.Zip(lhs, rhs) =>
          val (l, i) = fromPath(lhs, index)
          val (r, j) = fromPath(rhs, i)
          Zip(l, r) -> j
        case Path.ZipRight(lhs, rhs) =>
          val (l, i) = fromPath(lhs, index)
          val (r, j) = fromPath(rhs, i)
          Zip(l, r) -> j
      }

    // Adds completion to end of Opt
    def addCompletion(opt: Opt): Opt =
      opt match {
        case Zip(left, right) => Zip(left, addCompletion(right))
        case OrElse(_, _)     => throw new Error("THIS SHOULD NOT BE HERE")
        case Complete(_)      => throw new Error("THIS SHOULD NOT BE HERE!")
        case other            => Complete(other)
      }

    def make[A](path: Path[A]): Opt = {
      val (opt, _) = Opt.fromPath(path, 0)
      addCompletion(opt.rightAssociate)
    }
  }

  import Path._

  val optUsers       = Opt.make("users" / int)
  val optUsersPosts  = Opt.make("users" / int / "posts" / string)
  val usersPostsDone = Opt.make("users" / int / "posts")

  val missing: Opt     = Opt.make("missing")
  val missingPath: Opt = Opt.make("missing" / int)

  // OPTIMIZED PATH PARSING STRUCTURE
  //
  // OrElse(
  //  Zip(
  //    Opt.Complete(Literal(0,missing)),
  //    Opt.Complete(Parse(0,zio.api.Parser$$anon$2@1ee807c6))
  //  ),
  //  Zip(
  //    Literal(0, users),
  //    Zip(
  //      Opt.Complete(Parse(0,zio.api.Parser$$anon$2@1ee807c6)),
  //      Zip(
  //        Opt.Complete(Literal(1,posts)),
  //        Opt.Complete(Parse(1,zio.api.Parser$$anon$1@76a4d6c))
  //      )
  //    )
  //  )
  //)
  val eithered = optUsers | missing | missingPath | usersPostsDone | optUsersPosts

  def main(args: Array[String]): Unit = {
    println(eithered)
    println(eithered.render)
    println(eithered.run(List("users", "12", "posts", "hello")))
  }

}
