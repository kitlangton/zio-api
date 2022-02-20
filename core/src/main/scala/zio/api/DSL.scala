package zio.api

import zio.api

object DSL {
  // def interpret(dsl: DSL): Meaning

  // DSL => Semantics
  // DSL => (Int => Boolean)
  // DSL => String
  type Semantics = Int => Boolean

  def interpret(dsl: MatcherExec): Int => Boolean =
    dsl.run

  def interpret(dsl: MatcherDecl): Int => Boolean =
    dsl match {
      case MatcherDecl.Number(num) =>
        int => int == num
      case MatcherDecl.Or(left, right) =>
        int => interpret(left)(int) || interpret(right)(int)
    }

  def prettyPrint(dsl: MatcherDecl): String =
    dsl match {
      case MatcherDecl.Number(num) => num.toString
      case MatcherDecl.Or(left, right) =>
        s"${prettyPrint(left)} || ${prettyPrint(right)}"
    }

  def prettyPrint(dsl: MatcherExec): String =
    s"Exec... ${dsl.run}"

  sealed trait MatcherDecl { self =>
    def ||(that: MatcherDecl): MatcherDecl =
      MatcherDecl.Or(self, that)
  }

  object MatcherDecl {
    val one: MatcherDecl              = Number(1)
    val two: MatcherDecl              = Number(2)
    def number(int: Int): MatcherDecl = Number(int)

    final case class Number(int: Int)                          extends MatcherDecl
    final case class Or(left: MatcherDecl, right: MatcherDecl) extends MatcherDecl
  }

  final case class MatcherExec(run: Int => Boolean) {
    def ||(that: MatcherExec): MatcherExec =
      MatcherExec { int =>
        run(int) || that.run(int)
      }
  }

  object MatcherExec {
    val one              = number(1)
    val two              = number(2)
    def number(num: Int) = MatcherExec(int => int == num)
  }

  def main(args: Array[String]): Unit = {
    val dsl =
      MatcherExec.two || MatcherExec.one || MatcherExec.number(10)

    println(dsl)
    println(prettyPrint(dsl))

    val interpretation: Int => Boolean =
      interpret(dsl)

    println(interpretation(1))
    println(interpretation(2))
    println(interpretation(10))
    println(interpretation(11))
    println(interpretation(-1))
    println(interpretation(-2))
    println(interpretation(-10))
    println(interpretation(-11))
//    println(prettyPrint(MatcherDecl.one))
  }

}
