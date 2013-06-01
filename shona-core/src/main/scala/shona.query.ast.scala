//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package query

package ast {
  sealed trait Tree
  case class Property(name: String) extends Tree
  case class Select(qualifier: Tree, field: Property) extends Tree
  case class Apply(tree: Tree, operation: Operation) extends Tree

  sealed trait Operation
  case class MapOperation(mappings: Seq[Mapping]) extends Operation

  sealed trait Mapping { def tree: Tree }
  object Mapping {
    case class Identity(tree: Tree) extends Mapping
    case class Qualified(tree: Tree, name: String) extends Mapping
  }
}

package object ast {
  val Root = Property("@")
}
