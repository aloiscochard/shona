package shona {

  import scala.util.parsing.combinator.RegexParsers

  object Graph {
    sealed trait Node

    trait Entity extends Node {
      def name: String
      def properties: Seq[Property]
      def connections: Seq[Connection]
    }

    object Entity {
      def apply(n: String, ps: Seq[Property], cs: Seq[Connection]) = 
        new Entity { val name = n; val properties = ps; val connections = cs; }
    }

    trait Property extends Node {
      def name: String
    }

    object Property { def apply(n: String) = new Property { val name = n } }

    trait Connection extends Node {
      def name: String
      def target: Entity
    }

    object Connection { def apply(n: String, t: => Entity) = new Connection { val name = n; def target = t } }
  }

  package path {
    import Graph._
    import Expression._

    sealed trait Segment
    case class Element(node: Node) extends Segment
    case class View(fields: Seq[Path]) extends Segment

    object Path {
      // TODO Return Validation, error contain all not found path
      def apply(entity: Entity)(expression: Tree): Option[Path] = expression match {
        case Name(name) => 
          entity.properties.find(_.name == name).orElse(
            entity.connections.find(_.name == name)
          ).map(Element).map(_ :: Nil)

        case Fields(trees) => trees.flatMap(apply(entity)(_)) match {
          case xs if trees.size == xs.size => Some(View(xs) :: Nil)
          case _ => None
        }

        case Select(Name(name), tree) => 
          entity.connections.find(_.name == name).flatMap { connection =>
            apply(connection.target)(tree).map(Element(connection) +: _)
          }
      }
    }
  }



  object Expression extends RegexParsers {
    trait Label { def label: String }
    sealed trait Tree
    case class Name(value: String) extends Tree with Label { def label = value }
    case class Select(name: Name, value: Tree) extends Tree with Label { def label = name.value }
    case class Fields(values: Seq[Tree with Label]) extends Tree

    object Tree {
      def apply(name: Name, fields: Option[Fields]) = fields.fold[Tree with Label](name)(Select(name, _))
    }

    def apply(input: String): Either[String, Tree] = parseAll(fields | expression, input) match {
      case Success(tree, _) => Right(tree)
      case NoSuccess(message, _) => Left(message)
    }

    def expression: Parser[Tree with Label] = (rep1sep(name, ".").map(_.reverse) ~ fields.?).map(_ match {
      case (head :: Nil) ~ fields => Tree(head, fields)
      case (head :: xs) ~ fields => xs.foldLeft(Tree(head, fields))((tree, name) => Select(name, tree))
    })

    def fields : Parser[Fields] = ("(" ~> rep1sep(expression, ",")  <~ ")").map(Fields(_))
    def name: Parser[Name] = ("""\w+""".r).map(Name)
  }
}

package object shona {
  type Path = Seq[shona.path.Segment]
}
