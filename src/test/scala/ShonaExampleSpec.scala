package shona
package relational

import org.specs2.mutable._

import path._
//import Graph._

import org.json4s._

class ShonaExampleSpec extends Specification {
  val barName = Expression("barName").right.get
  val barAge = Expression("barAge").right.get

  val fooName = Expression("fooName").right.get
  val fooAge = Expression("fooAge").right.get

  val graphName = Expression("name").right.toOption.map(Path(SocialModel.person)).get
  val graphAge = Expression("age").right.toOption.map(Path(SocialModel.person)).get

  val barMapping = Seq(
    graphName -> barName,
    graphAge -> barAge
  )

  val fooMapping = Seq(
    graphName -> fooName,
    graphAge -> fooAge
  )

  //def foo[A, B, T](xs0: Map[T, A], xs1: Map[T, B]): Seq[A, B] =

  "ShonaExample" should {

    "schema mapping" in {

      val foo = Seq(
        fooName -> JString("alice"),
        fooAge -> JInt(20)
      )






      true === true
    }
  }
}
