package shona

import PartialFunction._

import org.json4s.JsonAST._
import org.json4s.JsonDSL._

import Expression._

object Json {
  def extract(json: JObject)(expression: Tree): Option[JValue] = expression match {
    case Name(name) => (json \ name) match {
      case JNothing => None
      case x => Some(x)
    }

    case Fields(trees) => trees.flatMap(tree => extract(json)(tree).map(tree.label -> _)) match {
      case Nil => None
      case xs => Some(JObject(xs:_*))
    }

    case Select(Name(name), tree) => (json \ name) match {
      case x: JObject => extract(x)(tree)
      case JArray(xs) => xs.flatMap { case x: JObject => extract(x)(tree); case _ => None } match {
        case Nil => None
        case xs => Some(JArray(xs))
      }
      case _ => None
    }
  }

  def render(xs: Seq[(Path, Tree)]): JObject = {
    var fields = xs
    while(fields.nonEmpty) {
      val (path, tree) = fields.head
      fields = fields.tail

      val head :: xs = path
      val brothers = fields.filter { case (path, tree) => path.head == head }
      fields = fields.diff(brothers)

      ((path, tree) :: brothers) 
    }
    null
  }
}
