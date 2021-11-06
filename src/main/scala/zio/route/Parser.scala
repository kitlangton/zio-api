package zio.route

trait Parser[A] {
  def parse(input: String): Option[A]
}

object Parser {
  implicit val stringParser: Parser[String] = new Parser[String] {
    override def parse(input: String): Option[String] = Some(input)
  }

  implicit val intParser: Parser[Int] = new Parser[Int] {
    override def parse(input: String): Option[Int] = input.toIntOption
  }

  implicit val booleanParser: Parser[Boolean] = new Parser[Boolean] {
    override def parse(input: String): Option[Boolean] = input.toBooleanOption
  }
}
