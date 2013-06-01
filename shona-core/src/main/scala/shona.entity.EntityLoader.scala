//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package entity

import shapeless.{BasisConstraint, HList, HNil, ToList, LUBConstraint}
import LUBConstraint._

import graph._
import record._

abstract class EntityLoader[N <: String, Properties <: HList : <<:[AnyProperty]#λ](vertex: Vertex[N, Properties]) {
  def all[PList <: HList, FList <: HList](properties: PList)(implicit 
    lub: LUBConstraint[PList, AnyProperty],
    rtr: RecordTRel[PList, Property, FList, Field],
    basis: BasisConstraint[PList, Properties],
    tl: ToList[PList, AnyProperty] 
  ): Seq[Entity[FList]] 
}

object EntityLoader {
  def apply[N <: String, Properties <: HList : <<:[AnyProperty]#λ](
    vertex: Vertex[N, Properties]
  )(implicit
    loader: EntityLoader[N, Properties]
  ) = loader

  def fromSeq[N <: String, Properties <: HList, Fields <: HList](
    vertex: Vertex[N, Properties], 
    xs: Seq[Entity[Fields]]
  )(implicit 
    propertiesLub: LUBConstraint[Properties, AnyProperty],
    propertiesRtr: RecordTRel[Properties, Property, Fields, Field],
    fieldsTl: ToList[Fields, Field[_ <: String, _]] 
  ): EntityLoader[N, Properties] = new EntityLoader(vertex) {

    override def all[PList <: HList, FList <: HList](properties: PList)(implicit 
      plub: LUBConstraint[PList, AnyProperty],
      rtr: RecordTRel[PList, Property, FList, Field],
      basis: BasisConstraint[PList, Properties],
      tl: ToList[PList, AnyProperty]
    ): Seq[Entity[FList]] = {
      val propertiesList = properties.toList[AnyProperty]
      xs.map { x =>
        // TODO Investigate adding LUBConstraint[FList, Field[_, _]] and cast at HList level
        new Entity(
          x.fields.toList
            .filter(field => propertiesList.find(x => x.label == field.label).isDefined)
            .foldRight[HList](HNil)(shapeless.::(_, _))
        )(null).asInstanceOf[Entity[FList]]
      }
    }
  }
}
