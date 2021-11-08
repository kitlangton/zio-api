package zio.route.macros

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

trait Matches[A, B]

object Matches {
  implicit def matches[A, B]: Matches[A, B] =
    macro Macros.matchesImpl[A, B]
}

object Macros {}

class Macros(val c: blackbox.Context) {
  import c.universe._

  def matchesImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: c.Expr[Matches[A, B]] = {
    val endpointTypes = getIntersectionIdents(weakTypeOf[A]).toSet
    println(weakTypeOf[B].widen)
    val handlerTypes = getIntersectionIdents(weakTypeOf[B]).toSet

    val extra   = handlerTypes -- endpointTypes
    val missing = endpointTypes -- handlerTypes

    reportMissing("Missing", missing)
    reportMissing("Extra", missing)

//    if (extra.nonEmpty || missing.nonEmpty) {
//      c.error(c.enclosingPosition, message)
//    }

    c.Expr[Matches[A, B]](q"""
      new Matches[${c.weakTypeOf[A]}, ${c.weakTypeOf[B]}] {}
    """)
  }

  private def reportMissing(name: String, missing: Set[String]) =
    if (missing.nonEmpty) {

      val missingString = missing.map(cyan).mkString("\n  - ")

      val message =
        s"""
        
$title 
           
$name ${pluralize("handler", missing.size)} for ${pluralize("endpoint", missing.size)}:
  - $missingString
  
"""
      c.abort(c.enclosingPosition, message)
    }

  private def getIntersectionIdents(intersection: c.Type): List[String] =
    intersection.dealias.intersectionTypes.map(_.dealias).map {
      case TypeRef(tpe, _, _) =>
        val tpeString = tpe.dealias.toString
        val packageName =
          tpeString.split("\\.").dropRight(1).lastOption.getOrElse(throw new Error("Missing Type!"))
        packageName
      case _ =>
        throw new Error("Missing Type!")
    }

  implicit class TypeOps(self: Type) {

    def isAny: Boolean = self.dealias.typeSymbol == typeOf[Any].typeSymbol

    /** Given a type `A with B with C` You'll get back List[A,B,C]
      */
    def intersectionTypes: List[Type] =
      self.dealias match {
        case t: RefinedType =>
          t.parents.flatMap(_.intersectionTypes)
        case TypeRef(_, sym, _) if sym.info.isInstanceOf[RefinedTypeApi] =>
          sym.info.intersectionTypes
        case other =>
          List(other)
      }
  }

  private def pluralize(string: String, n: Int): String =
    if (n > 1) s"${string}s" else string

  private def cyan(string: String): String =
    Console.CYAN + string + Console.RESET

  private lazy val title = scala.Console.RED + scala.Console.REVERSED + s" Handler Error " + scala.Console.RESET
}
