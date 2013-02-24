package shona
package relational

import org.specs2.mutable._

import path._
import Graph._

class ShonaRelationalSpec extends Specification {
  "ShonaRelational" should {
    "support model as graph" in {
      import path._

      println(Path(SocialModel.person)(Expression("(name, albums(photos.name))").right.get))
      println(Path(SocialModel.album)(Expression("(person, photos.name)").right.get))
      true === true
    }

    "load relational entity" in {
      import org.json4s._

      val persons = Map(
        1 -> Map("name" -> JString("alice"), "age" -> JInt(20)),
        2 -> Map("name" -> JString("bob"), "age" -> JInt(30))
      )

      // TODO Test relation loading
      object SocialEntityLoader extends EntityLoader(SocialModel) {
        import SocialModel._

        def load(entity: Entity, properties: Seq[Property]) = {
          entity match {
            case SocialModel.person => persons.to[Seq].map { case (id, fields) => 
              JObject(("id" -> JInt(id) :: fields.toList).filter { case (name, _) => properties.find(_.name == name).isDefined })
            }
            case _ => Nil
          }
        }

        def find(entity: Entity, properties: Seq[Property], property: Property, values: Seq[JValue]) = ???
        def findAll(entity: Entity, properties: Seq[Property], property: Property, value: Seq[JValue]) = ???
      }

      def load(entity: SocialModel.Entity, expression: String) =
        Expression(expression).right.toOption
          .flatMap(Path(entity))
          .map(SocialEntityLoader(entity))

      import org.json4s.native.JsonMethods._
      println {
        load(SocialModel.person, "(name, age)").map(_.map(render).map(pretty).mkString)
      }
      true === true
    }
  }
}

object SocialModel extends Model {
  import Relation._

  val id = Property("id")
  val personId = Property("person_id")
  val albumId = Property("album_id")

  val person = Entity("person", Seq(id, Property("name"), Property("age")))
  val album = Entity("album", Seq(id, personId, Property("name")))
  val photo = Entity("photo", Seq(id, albumId, Property("name")))

  val relationship = Seq(
    Relation(OneToMany, "albums", person, id, "person", album, personId),
    Relation(OneToMany, "photos", album, id, "album", photo, albumId)
  )
}

