//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

import language.experimental.macros
import scala.reflect.macros.{Context, Macro}

// Credits to @milesabin for the SingeltonType implementation -> https://github.com/milessabin/shapeless/tree/topic/macro-paradise

package shona {
  case class Label[T](value: T)

  trait SingletonTypeMacros extends Macro {
    import c.universe._

    def eval[T](t: c.Tree) = c.eval(c.Expr[T](c.resetAllAttrs(t.duplicate)))

    def singletonType[T](t: c.Expr[T]) =
      TypeTree(ConstantType(Constant(
        eval[T](t.tree)
      )))

    def witnessLabel[T: c.WeakTypeTag] = {
      weakTypeOf[T] match {
        case t @ ConstantType(Constant(s)) =>
          val T = TypeTree(t)
          val l = Literal(Constant(s))
          c.Expr[T](q"new Label[$T]($l)")
        case _ => c.abort(c.enclosingPosition, "Type argument must be a singleton type")
      }
    }
  }
}

package object shona {
  type label[T](t: T) = macro SingletonTypeMacros.singletonType[T]
  implicit def witnessLabel[T]: Label[T] = macro SingletonTypeMacros.witnessLabel[T]
  def singleton[T](implicit label: Label[T]): T = label.value
}
