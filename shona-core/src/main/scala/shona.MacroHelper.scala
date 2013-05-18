//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona

import scala.reflect.macros.Macro
import java.security.MessageDigest

trait MacroHelper extends Macro {
  import c._
  import c.universe._

  object Label {
    def unapply(tpe: Type) = {
      val ConstantType(Constant(id: String)) = tpe
      Some(id)
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

  private lazy val messageDigest = MessageDigest.getInstance("MD5")

  def digest(x: String): String = messageDigest.digest(x.getBytes).map("%02X".format(_)).mkString

  def enrich(tree: Tree, generatedCode: List[c.Tree]) = {
    val ClassDef(a, b, c, Template(parents, valDef, existingCode)) = tree
    ClassDef(a, b, c, Template(parents, valDef, existingCode ++ generatedCode))
  }
}
