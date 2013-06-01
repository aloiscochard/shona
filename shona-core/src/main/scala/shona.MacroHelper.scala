//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona

import scala.reflect.macros.Macro
import java.security.MessageDigest

import shona.query.Parser

trait MacroHelper extends Macro {
  import c._
  import c.universe._

  object Entity {
    def unapply(tpe: Type) = {
      val TypeRef(_, _, List(fieldsHList)) = tpe
      val fields = HList.decons(fieldsHList).map { x =>
        val TypeRef(_, _, List(Label(label), tpe)) = x
        label -> tpe
      }
      Some(fields)
    }
  }

  object Query {
    def fromTree(tree: Tree) = tree match {
      case Literal(Constant(input: String)) => Parser(input) match {
        case Right(query) => query
        case Left(message) => abort(tree.pos, message)
      }
      case query => abort(query.pos, "The query expression is not a string literal")
    }
  }

  object Graph {
    def unapply(tpe: Type) = {
      val TypeRef(_, _, xs) = tpe
      val List(verticesHList, edgesHList) = xs
      val vertices = HList.decons(verticesHList).map(withLabel)
      val edges = HList.decons(edgesHList).map(withLabel)
      Some(vertices -> edges)
    }
  }

  object HList {
    def decons(tpe: Type): List[Type] = tpe match {
      case TypeRef(x, _, a :: b :: _) => a :: decons(b)
      case x => Nil
    }

    def select(i: Int): Tree => Tree = tree => i match {
      case 0 => Select(tree, "head")
      case i => select(i - 1)(Select(tree, "tail"))
    }
  }

  object Label {
    def unapply(tpe: Type) = {
      val ConstantType(Constant(id: String)) = tpe
      Some(id)
    }
  }

  private lazy val messageDigest = MessageDigest.getInstance("MD5")

  def digest(x: String): String = messageDigest.digest(x.getBytes).map("%02X".format(_)).mkString

  def enrich(tree: Tree, generatedCode: List[c.Tree]) = {
    val ClassDef(a, b, c, Template(parents, valDef, existingCode)) = tree
    ClassDef(a, b, c, Template(parents, valDef, existingCode ++ generatedCode))
  }

  def withLabel(tpe: Type) = {
    val TypeRef(_, _, Label(label) :: _) = tpe
    label -> tpe
  }
}
