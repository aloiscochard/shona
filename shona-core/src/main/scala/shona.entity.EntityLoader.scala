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

abstract class EntityLoader[N <: String, Properties <: HList : <<:[Property[_, _]]#λ](val vertex: Vertex[N, Properties]) {
  def all[PList <: HList, FList <: HList](properties: PList)(implicit 
    lub: LUBConstraint[PList, Property[_, _]],
    rtr: RecordTRel[PList, Property, FList, Field],
    basis: BasisConstraint[PList, Properties],
    tl: ToList[PList, Property[_, _]] 
  ): Seq[Entity[FList]] 
}

object EntityLoader {

  object FieldToProperty extends RecordT[Field, Property] {
    def apply[N <: String, T](field: Field[N, T]): Property[N, T] = new Property[N, T](field.label)
  }

  def fromSeq[N <: String, Properties <: HList, Fields <: HList](
    vertex: Vertex[N, Properties], 
    xs: Seq[Entity[Fields]]
  )(implicit 
    propertiesLub: LUBConstraint[Properties, Property[_, _]],
    propertiesRtr: RecordTRel[Properties, Property, Fields, Field],
    fieldsTl: ToList[Fields, Field[_ <: String, _]] 
  ): EntityLoader[N, Properties] = new EntityLoader(vertex) {

    override def all[PList <: HList, FList <: HList](properties: PList)(implicit 
      plub: LUBConstraint[PList, Property[_, _]],
      rtr: RecordTRel[PList, Property, FList, Field],
      basis: BasisConstraint[PList, Properties],
      tl: ToList[PList, Property[_, _]]
    ): Seq[Entity[FList]] = {
      val propertiesList = properties.toList[Property[_, _]]
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
