//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package query

import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {
  import ast._

  def apply(input: String): Either[String, Tree] = parseAll(expression, input) match {
    case Success(tree, _) => Right(tree)
    case NoSuccess(message, _) => Left(message)
  }

  def expression: Parser[Tree] = select | operations.map(_(Root))

  def select: Parser[Tree] = (rep1sep(property, ".") ~ operations.?).map {
    case (x :: xs) ~ operations => 
      val tree = xs.foldLeft(Select(Root, x))(Select(_, _))
      operations.fold[Tree](tree)(_(tree))
  }

  def property: Parser[Property] = """\w+""".r.map(Property(_))

  def operations: Parser[Tree => Tree] = rep1(operation).map(xs => (xs: @unchecked) match {
    case x :: xs => tree => xs.foldLeft(Apply(tree, x))(Apply(_, _)) 
  })

  def operation: Parser[Operation] = "[" ~> ("=" ~> mappings).map(MapOperation(_)) <~ "]"

  def mappings: Parser[Seq[Mapping]] = ("{" ~> rep1sep(mapping, ",") <~ "}") | mapping.map(_ :: Nil)

  def mapping: Parser[Mapping] = 
    ("""\w+""".r ~ ":" ~ select).map { case name ~ ":" ~ property => Mapping.Qualified(property, name) } |
    select.map(Mapping.Identity(_))
}

object Query {
  import language.experimental.macros
  import scala.reflect.macros.{Context, Macro}

  def apply(query: String) = macro QueryMacro.apply
  def parse(query: String): Either[String, ast.Tree] = Parser(query)
}

trait QueryMacro extends MacroHelper {
  import c._
  import c.universe._

  def apply(query: c.Expr[String]): c.Expr[ast.Tree] = Query.fromTree(query.tree) match {
    case Right(_) => reify(Parser(query.splice).right.get)
    case Left(message) => abort(query.tree.pos, message)
  }
}
