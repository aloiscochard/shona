package shona
package query

import org.specs2.mutable._

import ast._
import Mapping._

class QuerySpec extends Specification {
  "Shona Query Parser" should {
    "parse expression" in {
      Parser("foo") === Right(Select(Root, Property("foo")))
      Parser("foo.bar") === Right(Select(Select(Root, Property("foo")), Property("bar")))
      Parser("[=foo]") === Right(Apply(Root, List(Map(List(Identity(Select(Root, Property("foo"))))))))
      Parser("[=foz:foo]") === Right(Apply(Root, List(Map(List(Qualified(Select(Root, Property("foo")), "foz"))))))
      Parser("[={foo, bar}]") === Right(Apply(Root, List(Map(List(Identity(Select(Root, Property("foo"))), Identity(Select(Root, Property("bar"))))))))
      Parser("[={foz:foo, baz:bar}]") === Right(Apply(Root, List(Map(List(Qualified(Select(Root, Property("foo")), "foz"), Qualified(Select(Root, Property("bar")), "baz"))))))
      Parser("foo[=bar]") === Right(Apply(Select(Root, Property("foo")), List(Map(List(Identity(Select(Root, Property("bar"))))))))
    }
  }
}
