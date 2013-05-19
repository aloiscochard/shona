//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package graph

import org.specs2.mutable._

import record._
import Property._

class GraphSpec extends Specification {

  val venueV = Vertex[label("venue")] ~ (
    int   [label("id")], 
    string[label("name")]
  )

  val addressV = Vertex[label("address")] ~ (
    int   [label("venueId")], 
    string[label("street")], 
    string[label("city")]
  )

  val reviewV = Vertex[label("review")] ~ (
    int   [label("id")], 
    string[label("venueId")], 
    string[label("rating")], 
    string[label("comment")]
  )

  val graph = Graph(venueV, addressV, reviewV)(
    Edge[label("address")] ~ (venueV, addressV), 
    Edge[label("review")] ~ (venueV, reviewV)
  )

  "Shona Graph" should {
    "support static lookup of vertex" in {
      Graph.get(graph)(venue) === venueV
      Graph.get(graph)(address) === addressV
    }
  }
}
