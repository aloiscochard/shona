package shona
package demo

import shona.graph._
import Property._
import shona.record._

object Model {
  val film = Vertex[label("film")] ~ (
    int   [label("id")], 
    string[label("name")],
    string[label("directorId")]
  )

  val director = Vertex[label("director")] ~ (
    string[label("id")],
    string[label("name")]
  )

  val review = Vertex[label("review")] ~ (
    int   [label("filmId")], 
    int   [label("rating")], 
    string[label("annotation")]
  )

  // Show: If a vertex is removed, compilation fail because edge refer to an unexisting vertex.
  // Improvment: Mapping using name directly
  val graph = Graph(film, director, review)(
    Edge[label("director")] ~ (film, director, Mapping.identity(string[label("directorId")], string[label("id")])),
    Edge[label("review")]   ~ (film, review, Mapping.identity(int[label("id")], int[label("filmId")]))
  )
}


