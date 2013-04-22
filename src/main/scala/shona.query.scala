package shona

package query {

  package ast {
    sealed trait Tree
    case class Property(name: String) extends Tree
    case class Select(qualifier: Tree, field: Property) extends Tree
    case class Apply(tree: Tree, operation: Operation) extends Tree

    sealed trait Operation
    case class Map(mappings: Seq[Mapping]) extends Operation

    sealed trait Mapping { def tree: Tree }
    object Mapping {
      case class Identity(tree: Tree) extends Mapping
      case class Qualified(tree: Tree, name: String) extends Mapping
    }
  }

  package object ast {
    val Root = Property("@")
  }

  import scala.util.parsing.combinator.RegexParsers

  object Parser extends RegexParsers {
    import ast._

    def apply(input: String): Either[String, Tree] = parseAll(expression, input) match {
      case Success(tree, _) => Right(tree)
      case NoSuccess(message, _) => Left(message)
    }

    def expression: Parser[Tree] = select | operations.map(_(Root))

    def select: Parser[Tree] = (rep1sep(property, ".") ~ operations.?).map {
      case (x :: xs) ~ operations => 
        val tree = xs.foldLeft(Select(Root, x))(Select(_, _))
        operations.fold[Tree](tree)(_(tree))
    }

    def property: Parser[Property] = """\w+""".r.map(Property(_))

    def operations: Parser[Tree => Tree] = rep1(operation).map(xs => (xs: @unchecked) match {
      case x :: xs => tree => xs.foldLeft(Apply(tree, x))(Apply(_, _)) 
    })

    def operation: Parser[Operation] = "[" ~> ("=" ~> mappings).map(Map(_)) <~ "]"

    def mappings: Parser[Seq[Mapping]] = ("{" ~> rep1sep(mapping, ",") <~ "}") | mapping.map(_ :: Nil)

    def mapping: Parser[Mapping] = 
      ("""\w+""".r ~ ":" ~ select).map { case name ~ ":" ~ property => Mapping.Qualified(property, name) } |
      select.map(Mapping.Identity(_))

  }
}
