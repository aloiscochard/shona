package shona

import org.specs2.mutable._

class ShonaSpec extends Specification {

  "Shona" should {
    "parse expression" in {
      import Expression._

      Expression("foo") === 
        Right(Name("foo"))
      Expression("foo.bar") === 
        Right(Select(Name("foo"),Name("bar")))
      Expression("foo(bar)") === 
        Right(Select(Name("foo"),Fields(List(Name("bar")))))
      Expression("foo(foo, bar)") === 
        Right(Select(Name("foo"),Fields(List(Name("foo"), Name("bar")))))
      Expression("foo(foo.bar, bar.foo)") === 
        Right(Select(Name("foo"),Fields(List(Select(Name("foo"),Name("bar")), Select(Name("bar"),Name("foo"))))))
      Expression("foo(foo.bar(foo), bar.foo(bar))") ===
        Right(Select(Name("foo"),Fields(List(Select(Name("foo"),Select(Name("bar"),Fields(List(Name("foo"))))), Select(Name("bar"),Select(Name("foo"),Fields(List(Name("bar")))))))))

      Expression("(foo, bar)") === Right(Fields(List(Name("foo"), Name("bar"))))

      Expression("foo(") === 
        Left("""string matching regex `\w+' expected but end of source found""")
      Expression("foo(bar,)") ===
        Left("""string matching regex `\w+' expected but `)' found""")
    }
  }

  "convert expression to graph path" in {
    import Graph._
    import path._

    object SocialGraph {
      val friends: Connection = Connection("friends", person)
      val person: Entity = Entity("person", Seq(Property("name"), Property("age")), Seq(friends))
    }

    println(Path(SocialGraph.person)(Expression("friends(name, age, friends.name)").right.get))

    true === true
  }
}
