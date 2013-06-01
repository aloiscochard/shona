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

class EntityFinderSpec extends Specification {

  val venue = Vertex[label("venue")] ~ (
    int   [label("id")], 
    string[label("name")]
  )

  val finder = EntityFinder.fromMap(
    venue, 
    int[label("id")],
    Map(
      21 -> Entity((Field[label("id")] ~ 21, Field[label("name")] ~ "Pizzeria Mario")),
      42 -> Entity((Field[label("id")] ~ 42, Field[label("name")] ~ "El Tacos"))
    )
  )

  "Shona EntityFinder.fromMap" should {
    "find entities with every fields" in {
      val xs = finder.findAll(venue.properties, Seq(21, 42)).mapValues(View(_))
      xs(21).id === 21
      xs(21).name === "Pizzeria Mario"
      xs(42).id === 42
      xs(42).name === "El Tacos"
    }

    "find entities for given fields only" in {
      // TODO Once dynamic field selector implemented: prove that name is not part of the entities
      val xs = finder.findAll(string[label("name")] :: HNil, Seq(21, 42)).mapValues(View(_))
      xs(21).name === "Pizzeria Mario"
      xs(42).name === "El Tacos"
    }
  }
}


