package shona

import PartialFunction._

import org.json4s.JsonAST._
import org.json4s.JsonDSL._

import Expression._

object Json {
  def apply(json: JObject)(expression: Tree): Option[JValue] = expression match {
    case Name(name) => (json \ name) match {
      case JNothing => None
      case x => Some(x)
    }

    case Fields(trees) => trees.flatMap(tree => apply(json)(tree).map(tree.label -> _)) match {
      case Nil => None
      case xs => Some(JObject(xs:_*))
    }

    case Select(Name(name), tree) => (json \ name) match {
      case x: JObject => apply(x)(tree)
      case JArray(xs) => xs.flatMap { case x: JObject => apply(x)(tree); case _ => None} match {
        case Nil => None
        case xs => Some(JArray(xs))
      }
      case _ => None
    }
    /*
      entity.connections.find(_.name == name).flatMap { connection =>
        apply(connection.target)(tree).map(Element(connection) +: _)
      }
      */
  }
}
