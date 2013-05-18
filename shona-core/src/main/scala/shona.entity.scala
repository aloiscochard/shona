//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona

import language.experimental.macros
import scala.reflect.macros.{Context, Macro}

import shapeless.{HList, HListerAux, HNil, LUBConstraint}
import shapeless.LUBConstraint._

import record._

package entity {
  class Entity[Fields <: HList : <<:[Field[_, _]]#λ](val fields: Fields) {
    override def toString = s"Entity(${fields.toString})"
  }

  object Entity {
    def apply[N <: String, T](field: Field[N, T]) = new Entity(field :: HNil)
    def apply[P <: Product, L <: HList](p: P)(implicit hl : HListerAux[P, L], lub: LUBConstraint[L, Field[_, _]]): Entity[L] = 
      new Entity(hl(p))
  }

  class Field[N <: String, T](val label: Label[N], val value: T) extends Record[N, T] {
    override def toString = s"${label.value}(${value})"
  }

  object Field extends RecordFactory[Field] {
    def create[N <: String, T](label: Label[N], value: T) = new Field(label, value)
  }

  object View {
    def apply[E <: Entity[_]](entity: E) = macro ViewMacro.apply[E]
  }

  trait ViewMacro extends Macro with MacroHelper {
    def apply[E <: Entity[_] : c.WeakTypeTag](entity: c.Expr[E]): c.Tree = {
      import c._
      import c.universe._

      val entityType = c.weakTypeOf[E]

      val fields = {
        val TypeRef(_, _, List(fieldsHList)) = entityType
        HList.decons(fieldsHList).map { x => 
          val TypeRef(_, _, List(Label(label), tpe)) = x
          TermName(label) -> TypeTree(tpe)
        }
      }

      val name = TypeName("View" + digest(fields.toString))

      if (c.typeCheck(q"new shona.entity.views.$name(${entity.tree})", silent = true) == EmptyTree) {
        c.introduceTopLevel("shona.entity.views",
          enrich(q"class $name(val entity: ${TypeTree(entityType)})", fields.zipWithIndex.map { case ((label, tpe), index) =>
            val body = Select(HList.select(index)(Select(Ident(TermName("entity")), TermName("fields"))), TermName("value"))
            q"def ${label}: ${tpe} = $body"
          })
        )
      }

      q"new shona.entity.views.$name(${entity.tree})"
    }
  }
}
