package zio.api.experimental

import zio.api._

import scala.annotation.switch
import scala.language.implicitConversions

/** TODO:
  *   - Add `Map` node
  *   - Integrate with actual Path type
  *   - Benchmark
  */
object Experiment {

  final case class State(
      path: Array[String],
      var index: Int,
      var parsed: Int,
      builder: Array[Any],
      length: Int
  ) {

    override def toString: String =
      s"State(path: ${path.toList}, index: $index, parsed: $parsed, builder: ${builder.toList})"

    def outOfBounds: Boolean = index >= length

  }

  sealed trait Opt extends Product with Serializable { self =>

    // TODO: Never throw away a Complete node. Change this to unify the nodes
    // return an Option[Opt] perhaps.
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
        case Opt.Literal(string) =>
          s"Literal($string)"
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
    final case class Literal(string: String) extends Opt {
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
        if (state.index >= state.length) {
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
        val res = opt.runImpl(state)
        if (res == null || state.index != state.length) return null
        val result = arrayToTuple(state.parsed, state.builder)
        result
      }
    }

    def fromPath[A](path: Path[A], index: Int): (Opt, Int) =
      path match {
        case Path.Literal(value) =>
          Literal(value) -> index
        case Path.Match(_, parser, _) =>
          Parse(index, parser) -> (index + 1)
        case Path.ZipWith(lhs, rhs, _, _) =>
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

    def run(input: List[String], opt: Opt): Any = {
      val state = State(input.toArray, 0, 0, Array.ofDim(input.size), input.length)

      def loop(opt: Opt): Any =
        (opt: Opt @switch) match {
          case Literal(string) =>
            if (!state.outOfBounds && state.path(state.index) == string)
              state.index += 1
            else
              null

          case Parse(index, parser) =>
            if (state.index >= state.length) {
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

          case Zip(left, right) =>
            val result = loop(left)
            if (result != null && left.isInstanceOf[Complete]) result
            else loop(right)

          case OrElse(left, right) =>
            val index  = state.index
            val parsed = state.parsed
            val result = loop(left)
            if (result != null) return result
            state.index = index
            state.parsed = parsed
            loop(right)

          case Complete(opt) =>
            val res = loop(opt)
            if (res == null || state.index != state.length) return null
            val result = arrayToTuple(state.parsed, state.builder)
            result
        }

      loop(opt)
    }
  }

  val optUsers       = Opt.make("users" / int)
  val optUsersPosts  = Opt.make("users" / int / "posts" / string)
  val usersPostsDone = Opt.make("users" / int / "posts")

  val missing: Opt     = Opt.make("missing")
  val missingPath: Opt = Opt.make("missing" / int)

  // OPTIMIZED PATH PARSING STRUCTURE
  //
  // OrElse(
  //  Zip(
  //    Opt.Complete(Literal(missing)),
  //    Opt.Complete(Parse(0,zio.api.Parser$$anon$2@1ee807c6))
  //  ),
  //  Zip(
  //    Literal(users),
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

  private def arrayToTuple(size: Int, array: Array[Any]): Any =
    // format: off
    size match {
      case 0  => ()
      case 1  => array(0)
      case 2  => (array(0), array(1))
      case 3  => (array(0), array(1), array(2))
      case 4  => (array(0), array(1), array(2), array(3))
      case 5  => (array(0), array(1), array(2), array(3), array(4))
      case 6  => (array(0), array(1), array(2), array(3), array(4), array(5))
      case 7  => (array(0), array(1), array(2), array(3), array(4), array(5), array(6))
      case 8  => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7))
      case 9  => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8))
      case 10 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9))
      case 11 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10))
      case 12 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11))
      case 13 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12))
      case 14 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13))
      case 15 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14))
      case 16 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15))
      case 17 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16))
      case 18 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17))
      case 19 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18))
      case 20 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18), array(19))
      case 21 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18), array(19), array(20))
      case 22 => (array(0), array(1), array(2), array(3), array(4), array(5), array(6), array(7), array(8), array(9), array(10), array(11), array(12), array(13), array(14), array(15), array(16), array(17), array(18), array(19), array(20), array(21))
    }
    // format: on

}
