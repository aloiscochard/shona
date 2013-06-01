//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package engine

import org.specs2.mutable._

import shapeless._

import record._
import graph._
import Property._
import entity._

class ExecutionEngineSpec extends Specification {

  val venue = Vertex[label("venue")] ~ (
    int   [label("id")], 
    string[label("name")]
  )

  val address = Vertex[label("address")] ~ (
    int   [label("venueId")], 
    string[label("street")], 
    string[label("city")]
  )

  val venueToAddress = Edge[label("address")] ~ (venue, address, Mapping.identity(int[label("id")], int[label("venueId")]))

  val graph = Graph(venue :: address :: HNil, venueToAddress :: HNil)

  val venues = Seq(
    Entity((Field[label("id")] ~ 21, Field[label("name")] ~ "Pizzeria Mario")),
    Entity((Field[label("id")] ~ 42, Field[label("name")] ~ "El Tacos"))
  )

  val addresses = Seq(
    Entity((Field[label("venueId")] ~ 21, Field[label("street")] ~ "Italian Road", Field[label("city")] ~ "London")),
    Entity((Field[label("venueId")] ~ 42, Field[label("street")] ~ "Mexican Road", Field[label("city")] ~ "London"))
  )

  implicit val venueLoader = EntityLoader.fromSeq(venue, venues)
  implicit val addressFinder = EntityFinder.fromFunction(address, int[label("venueId")])(x => addresses.find(_.fields.head.value == x))


  // TODO Remove '.toString' once equality support is implemented
  "Shona ExecutionEngine" should {
    "load a vertex" in {
      Execute(graph)("venue").toString === venueLoader.all(venue.properties).toString
    }

    "load a vertex property" in {
      Execute(graph)("venue.name").toString === venueLoader.all(string[label("name")] :: HNil).toString
      Execute(graph)("venue.id").toString === venueLoader.all(int[label("id")] :: HNil).toString
    }

    "load a vertex edge" in {
      Execute(graph)("venue.address").toString === 
        addressFinder.findAll(address.properties, Seq(21, 42)).values.toString
    }

    "load a vertex edge property" in {
      Execute(graph)("venue.address.street").toString === 
        addressFinder.findAll(string[label("street")] :: HNil, Seq(21, 42)).values.toString
    }

    "load a vertex with mapping" in {
      val List(a, b) = Execute(graph)("venue[={name, address.street}]").map(View(_))
      a.name === "Pizzeria Mario"
      a.street === "Italian Road"
      b.name === "El Tacos"
      b.street === "Mexican Road"
    }

    // TODO Support recusive map operations
    /*
    "load a vertex with mapped property and edge recursively" in {
    }
    */
  }
}


