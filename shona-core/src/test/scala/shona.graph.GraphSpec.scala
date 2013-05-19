//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package graph

import org.specs2.mutable._

import record._

class GraphSpec extends Specification {

  val venueV = Vertex[label("venue")] ~ (
    Property.Int[label("id")], 
    Property.String[label("name")]
  )

  val addressV = Vertex[label("address")] ~ (
    Property.Int[label("venueId")],
    Property.String[label("street")], 
    Property.String[label("city")]
  )

  val reviewV = Vertex[label("review")] ~ (
    Property.String[label("id")], 
    Property.String[label("venueId")], 
    Property.String[label("rating")],
    Property.String[label("comment")]
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
