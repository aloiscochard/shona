//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona

import language.experimental.macros
import scala.reflect.macros.{Context, Macro}

import graph._
import query.ast

package engine {
  object Execute {
    def apply[G <: Graph[_, _]](graph: G)(query: String) = macro ExecuteMacro.apply[G]
  }

  trait ExecuteMacro extends MacroHelper {
    import c._
    import c.universe._

    def apply[G <: Graph[_, _] : c.WeakTypeTag](graph: c.Expr[G])(query: c.Expr[String]): Tree = {

      def extractPath(x: ast.Tree): Seq[String] = x match {
        case ast.Select(ast.Root, ast.Property(name)) => Seq(name)
        case ast.Select(tree, ast.Property(name)) => extractPath(tree) :+ name
        case _ => ???
      }

      val Graph(vertices, edges) = graph.tree.tpe

      def findVertex(name: String) = indexOf(vertices, name).map { case (index, tpe) =>
        val vertex = HList.select(index)(q"${graph.tree}.vertices")
        val Vertex(properties) = tpe
        val entityLoader =  q"shona.entity.EntityLoader(${vertex})"
        (vertex, entityLoader, properties)
      }

      def findProperty(vertex: Tree, properties: Seq[(String, Type)], name: String) = indexOf(properties, name).map {
        case (index, tpe) => HList.select(index)(q"${vertex}.properties") -> tpe
      }

      def findEdge(name: String) = indexOf(edges, name).map { case (index, tpe) =>
        HList.select(index)(q"${graph.tree}.edges") -> tpe
      }

      def get(path: Seq[String]) = 
        path match {
          case name :: tail => findVertex(name) match {
            case Some((vertex, entityLoader, properties)) => 
              tail match {
                case Nil => 
                  q"${entityLoader}.all(${vertex}.properties)"

                case name :: tail => (findProperty(vertex, properties, name), tail) match {
                  case (Some((property, _)), Nil) => q"${entityLoader}.all(shapeless.HNil.::(${property}))"
                  case (None, tail) => findEdge(name) match {
                    case Some((edge, tpe)) => 
                      val ids = q"${entityLoader}.all(shapeless.HNil.::(${edge}.mapping.from)).map(_.fields.head.value)"
                      val finder = q"shona.entity.EntityFinder(${edge}.to, ${edge}.mapping.to)"

                      tail match {
                        case Nil => 
                          q"${finder}.findAll(${edge}.to.properties, ${ids}).values"

                        case name :: Nil => 
                          val Edge(_, _, _, properties, _) = tpe
                          indexOf(properties, name) match {
                            case Some((index, _)) =>
                              val property = HList.select(index)(q"${edge}.to.properties")
                              q"${finder}.findAll(shapeless.HNil.::(${property}), ${ids}).values"
                            case _ => ???
                          }
                        case _ => ???
                      }
                    case _ => ???
                  }
                  case _ => ???
                }
                case _ => ???
              }
            case _ => ???
          }
          case _ => ???
        }

      Query.fromTree(query.tree) match {
        case ast.Apply(ast.Select(ast.Root, ast.Property(name)), ast.MapOperation(pathsTree)) => findVertex(name) match {
          case Some((vertex, entityLoader, vertexProperties)) =>
            val paths = pathsTree.map(_.tree).map(extractPath)

            val (propertiesPath, edgesPath) = paths.partition(_.size == 1)
            val propertiesWithType = propertiesPath.map(_.head).flatMap(findProperty(vertex, vertexProperties, _))
            val properties = propertiesWithType.map(_._1)
            val edgesWithType = edgesPath.map(_.head).flatMap(findEdge)
            val edgesProperties = edgesWithType.map { case (edge, _) => q"${edge}.mapping.from" }

            def toHList(xs: Seq[Tree]) = xs.foldLeft[Tree](q"shapeless.HNil")((acc, x) => q"${acc}.::(${x})")
            val loaderProperties = toHList((properties ++ edgesProperties).distinct)

            /*
            val foo = edgesWithType.map { case (edge, edgeTpe) =>
              val Edge(Label(name), Label(propertyFromName), _, _) = edgeTpe
              val finder = q"shona.entity.EntityFinder(${edge}.to, ${edge}.mapping.to)"
              val ids = q"entities.map(View(_)).map(_.${TermName(propertyFromName)})"
              q"val ${TermName(name)} = ${finder}.findAll(${edge}.to.properties, ${ids})"
            }
            */

            val edgeFields = edgesWithType.zip(edgesPath).map { case ((edge, edgeTpe), edgePath) =>
              val Edge(Label(name), _, Label(propertyFromName), propertiesTo, _) = edgeTpe
              val finder = q"shona.entity.EntityFinder(${edge}.to, ${edge}.mapping.to)"
              val id = q"shona.entity.View(entity).${TermName(propertyFromName)}"
              val select = edgePath match {
                case _ :: Nil => q"${edge}.to.properties"
                case _ :: propertyName :: _ => indexOf(propertiesTo, propertyName) match {
                  case Some((index, _)) => 
                    val property = HList.select(index)(q"${edge}.to.properties")
                    q"shapeless.HNil.::(${property})"
                  case _ => ???
                }
                case _ => ???
              }
              q"${finder}.find(${select}, ${id}).get.fields"
            }

            val propertiesFields = propertiesPath.map(_.head).flatMap { name =>
              indexOf(vertexProperties, name).map { case (index, tpe) =>
                HList.select(index)(q"entity.fields")
              }
            }

            //val propertiesFields = propertiesPath.map(_.head).map(name => q"View(entity).${TermName(name)}")
            val mappedEntityFields = (toHList(propertiesFields) +: edgeFields).reduceLeft { (a, b) =>
              q"${a} ::: ${b}"
            }

            val mappedEntity = q"new shona.entity.Entity(${mappedEntityFields})"

            q"${entityLoader}.all(${loaderProperties}).map(entity => ${mappedEntity})"
          case _ => ???
        }
        case x => get(extractPath(x))
      }
    }
  }
}
