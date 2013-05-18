//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package query

import org.specs2.mutable._

import ast._
import Mapping._

class QuerySpec extends Specification {
  "Shona Query Parser" should {
    "compile expression" in {
      Query("foo[=bar]") === Apply(Select(Root, Property("foo")), MapOperation(List(Identity(Select(Root, Property("bar"))))))
    }

    "parse expression" in {
      Query.parse("foo") === Right(Select(Root, Property("foo")))
      Query.parse("foo.bar") === Right(Select(Select(Root, Property("foo")), Property("bar")))
      Query.parse("[=foo]") === Right(Apply(Root, MapOperation(List(Identity(Select(Root, Property("foo")))))))
      Query.parse("[=foz:foo]") === Right(Apply(Root, MapOperation(List(Qualified(Select(Root, Property("foo")), "foz")))))
      Query.parse("[={foo, bar}]") === Right(Apply(Root, MapOperation(List(Identity(Select(Root, Property("foo"))), Identity(Select(Root, Property("bar")))))))
      Query.parse("[={foz:foo, baz:bar}]") === Right(Apply(Root, MapOperation(List(Qualified(Select(Root, Property("foo")), "foz"), Qualified(Select(Root, Property("bar")), "baz")))))
      Query.parse("foo[=bar]") === Right(Apply(Select(Root, Property("foo")), MapOperation(List(Identity(Select(Root, Property("bar")))))))
    }
  }
}
