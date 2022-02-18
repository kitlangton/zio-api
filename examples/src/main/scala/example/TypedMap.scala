//package example
//
//object Demo {
//
//  trait WithPath[A]  {}
//  trait WithQuery[A] {}
//
//  final case class Endpoint[A]()
//
//  trait Handle[A] {
//    type Out
//  }
//
//  object Handle extends LowPriorityHandle {
//    implicit def handleBoth[A, Path, Query](implicit
//        ev: A <:< WithPath[Path],
//        ev2: A <:< WithQuery[Query]
//    ): Handle[A] { type Out = (Path, Query) } =
//      new Handle[A] {
//        type Out = (Path, Query)
//      }
//  }
//
//  trait LowPriorityHandle {
//    implicit def handlePath[A, Path](implicit ev: A <:< WithPath[Path]): Handle[A] { type Out = Path } =
//      new Handle[A] {
//        type Out = Path
//      }
//
//    implicit def handleQuery[A, Query](implicit ev: A <:< WithQuery[Query]): Handle[A] { type Out = Query } =
//      new Handle[A] {
//        type Out = Query
//      }
//  }
//
//  def handle[R, Path](endpoint: Endpoint[R])(implicit ev: Handle[R]): ev.Out => Unit =
//    a => println(a)
//
//  val endpoint = Endpoint[WithPath[(String, Int)] with WithQuery[Int]]()
//  val handler  = handle(endpoint)
//  handler((("hello", 42), 42))
//
//  val cool  = Path.named[String]("name")
//  val again = Path.named[Int]("age")
//  val both: Path[("name" -> String) with ("age" -> Int)] =
//    cool ++ again
//
//  trait ->[+S <: Singleton with String, +A]
//
//  final case class TypedMap[+A](map: Map[String, Any]) {
//    def +[S <: Singleton with String, B](name: S, value: B): TypedMap[A with (S -> B)] =
//      TypedMap(map + (name -> value))
//
//    def apply[S <: Singleton with String, B](name: S)(implicit ev: A <:< (S -> B)): B =
//      map(name).asInstanceOf[B]
//  }
//
//  object TypedMap {
//    def apply[S <: Singleton with String, B](name: S, value: B): TypedMap[S -> B] =
//      TypedMap(Map(name -> value))
//  }
//
//  val tmap: TypedMap[("name" -> String) with ("age" -> Int)] =
//    TypedMap("name", "kit") + ("age", 23)
//
//  def main(args: Array[String]): Unit = {
//    val name: String = tmap("name")
//    val age: Int     = tmap("age")
//    // val missing: Boolean = tmap("missing") ! does not compile
//    println(name)
//    println(age)
//  }
//
//  final case class NamedPartially[A]() {
//    def apply[S <: Singleton with String](name: S): Path[S -> A] = Path.Named[S, A](name)
//  }
//
//  sealed trait Path[+A] { self =>
//    def ++[B](that: Path[B]): Path[A with B] = Path.Both[A, B](this, that)
//
//    def parse(string: String): Option[TypedMap[A]] =
//      parseImpl(string.split("&").toList).map { case (_, map) =>
//        new TypedMap[A](map)
//      }
//
//    private def parseImpl(input: List[String]): Option[(List[String], Map[String, Any])] =
//      self match {
//        case named: Path.Named[_, _] =>
//          val name = named.name
//          input match {
//            case ::(s"${`name`}=$value", next) =>
//              Some(
//                next -> Map(name -> value)
//              )
//            case _ =>
//              None
//          }
//        case Path.Both(left, right) =>
//          for {
//            (next, leftMap)  <- left.parseImpl(input)
//            (next, rightMap) <- right.parseImpl(next)
//          } yield (next, leftMap ++ rightMap)
//      }
//  }
//
//  object Path {
//
//    def named[A]: NamedPartially[A] = NamedPartially[A]()
//
//    final case class Named[+S <: Singleton with String, +A](name: S) extends Path[S -> A]
//
//    final case class Both[+A, +B](left: Path[A], right: Path[B]) extends Path[A with B]
//  }
//}
