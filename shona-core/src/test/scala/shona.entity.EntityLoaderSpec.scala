//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package entity

import org.specs2.mutable._

import shapeless._

import record._
import graph._
import Property._

class EntityLoaderSpec extends Specification {

  val venue = Vertex[label("venue")] ~ (
    int   [label("id")], 
    string[label("name")]
  )

  val loader = EntityLoader.fromSeq(venue, 
    List(
      Entity((Field[label("id")] ~ 21, Field[label("name")] ~ "Pizzeria Mario")),
      Entity((Field[label("id")] ~ 42, Field[label("name")] ~ "El Tacos"))
    )
  )

  "Shona EntityLoader.fromSeq" should {
    "load all entities with every fields" in {
      val List(a, b) = loader.all(venue.properties).map(View(_))
      a.id === 21
      a.name === "Pizzeria Mario"
      b.id === 42
      b.name === "El Tacos"
    }

    "load all entities for given fields only" in {
      // TODO Once dynamic field selector implemented: prove that name is not part of the entities
      val List(a, b) = loader.all(int[label("id")] :: HNil).map(View(_))
      a.id === 21
      b.id === 42
    }
  }
}

