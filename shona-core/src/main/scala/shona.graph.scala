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

package graph {

  class Graph[Vertices <: HList : <<:[Vertex[_, _]]#λ, Edges <: HList : <<:[Edge[_, _, _]]#λ](
    val vertices: Vertices,
    val edges: Edges
  ) 

  object Graph {
    def apply[VP <: Product, VL <: HList, EP <: Product, EL <: HList](vp: VP)(ep: EP)(implicit
      vhl: HListerAux[VP, VL], vlub: LUBConstraint[VL, Vertex[_, _]],
      ehl: HListerAux[EP, EL], elub: LUBConstraint[EL, Edge[_, _, _]]
    ) = new Graph(vhl(vp), ehl(ep))

    // TODO Investigate implicits based implementation (vs macro)
    def get[G <: Graph[_, _]](graph: G)(path: _) = macro GraphMacro.get[G]
  }

  trait GraphMacro extends Macro with MacroHelper {
    def get[G <: Graph[_, _] : c.WeakTypeTag](graph: c.Expr[G])(path: c.Tree): c.Tree = {
      import c._
      import c.universe._

      val Graph(vertices, _) = c.typeCheck(graph.tree).tpe

      path match {
        case Ident(TermName(vertexLabel)) =>
          vertices.map({ case (label, _) => label }).zipWithIndex.find({ case (label, _ ) => label == vertexLabel }) match {
            case Some((_, index)) => HList.select(index)(q"${graph.tree}.vertices")
            case None => c.abort(path.pos, s"Vertex '$vertexLabel' not found")
          }
        case _ => c.abort(path.pos, "Invalid path expression (only vertex label are currently supported)")
      }
    }
  }

  class Edge[N <: String, V1 <: Vertex[_, _], V2 <: Vertex[_, _]](name: Label[N], v1: V1, v2: V2)

  object Edge {
    class EdgeBuilder[N <: String](label: Label[N]) { 
      def ~[V1 <: Vertex[_, _], V2 <: Vertex[_, _]](v1: V1, v2: V2) = new Edge(label, v1, v2)
    }
    final def apply[N <: String]()(implicit label: Label[N]) = new EdgeBuilder(label)
  }

  sealed abstract class Property[N <: String, T](override val label: Label[N]) extends Record[N, T]
  object Property {
    case class Boolean[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, scala.Boolean](label)
    case class Byte[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, scala.Byte](label)
    case class Bytes[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, Array[scala.Byte]](label)
    case class Char[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, Array[scala.Char]](label)
    case class Double[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, scala.Double](label)
    case class Float[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, scala.Float](label)
    case class Int[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, scala.Int](label)
    case class Long[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, scala.Long](label)
    case class Short[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, scala.Short](label)
    case class String[N <: java.lang.String](implicit override val label: Label[N]) extends Property[N, java.lang.String](label)
  }

  class Vertex[N <: String, Properties <: HList : <<:[Property[_, _]]#λ](
    val name: Label[N], 
    val properties: Properties
  )

  object Vertex {
    class VertexBuilder[N <: String](label: Label[N]) { 
      //def ~[PN <: String, T](property: Property[PN, T]) = new Vertex(label, property :: HNil)
      def ~[P <: Product, L <: HList](p: P)(implicit hl: HListerAux[P, L], lub: LUBConstraint[L, Property[_, _]]): Vertex[N, L] = 
        new Vertex(label, hl(p))
    }
    final def apply[N <: String]()(implicit label: Label[N]) = new VertexBuilder(label)
  }
}
