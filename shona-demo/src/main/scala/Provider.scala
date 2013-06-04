package shona
package demo

import shona.graph._
import Property._
import shona.record._
import shona.entity._

object Provider {

  val films = Seq(
    Entity((Field[label("id")] ~ 1, Field[label("name")] ~ "In the Mood for Love", Field[label("directorId")] ~ "/m/01f7v_")),
    Entity((Field[label("id")] ~ 2, Field[label("name")] ~ "Empire of the Sun",    Field[label("directorId")] ~ "/m/06pj8"))
  )

  val directors = Seq(
    Entity((Field[label("id")] ~ "/m/01f7v_", Field[label("name")] ~ "Wong Kar-wai")),
    Entity((Field[label("id")] ~ "/m/06pj8",  Field[label("name")] ~ "Steven Spielberg"))
  )

  val reviews = Seq(
    Entity((Field[label("filmId")] ~ 1, Field[label("rating")] ~ 4, Field[label("annotation")] ~ "TBD")),
    Entity((Field[label("filmId")] ~ 2, Field[label("rating")] ~ 5, Field[label("annotation")] ~ "TBD"))
  )

  implicit val filmsLoader = EntityLoader.fromSeq(Model.film, films)
  implicit val directorsLoader = EntityFinder.fromFunction(Model.director, string[label("id")])(x => directors.find(_.fields.head.value == x))
  implicit val reviewsLoader = EntityFinder.fromFunction(Model.review, int[label("filmId")])(x => reviews.find(_.fields.head.value == x))
}
