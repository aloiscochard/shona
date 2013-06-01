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

abstract class EntityFinder[N <: String, Properties <: HList : <<:[AnyProperty]#λ, PN <: String, PT](
  vertex: Vertex[N, Properties], 
  property: Property[PN, PT]
)(implicit
  plub: LUBConstraint[Properties, AnyProperty],
  pbasis: BasisConstraint[shapeless.::[Property[PN, PT], HNil], Properties]
){
  def find[PList <: HList, FList <: HList](properties: PList, x: PT)(implicit 
    lub: LUBConstraint[PList, AnyProperty],
    rtr: RecordTRel[PList, Property, FList, Field],
    basis: BasisConstraint[PList, Properties],
    tl: ToList[PList, AnyProperty] 
  ): Option[Entity[FList]] = findAll(properties, Seq(x)).values.headOption
     
  def findAll[PList <: HList, FList <: HList](properties: PList, xs: Seq[PT])(implicit 
    lub: LUBConstraint[PList, AnyProperty],
    rtr: RecordTRel[PList, Property, FList, Field],
    basis: BasisConstraint[PList, Properties],
    tl: ToList[PList, AnyProperty] 
  ): Map[PT, Entity[FList]] 
}

object EntityFinder {
 def apply[N <: String, Properties <: HList : <<:[AnyProperty]#λ, PN <: String, PT](
    vertex: Vertex[N, Properties], 
    property: Property[PN, PT]
  )(implicit
    finder: EntityFinder[N, Properties, PN, PT]
  ) = finder

  def fromFunction[N <: String, Properties <: HList, Fields <: HList, PN <: String, PT](
    vertex: Vertex[N, Properties], 
    property: Property[PN, PT]
  )(
    f: PT => Option[Entity[Fields]]
  )(implicit 
    propertiesLub: LUBConstraint[Properties, AnyProperty],
    pbasis: BasisConstraint[shapeless.::[Property[PN, PT], HNil], Properties],
    propertiesRtr: RecordTRel[Properties, Property, Fields, Field],
    fieldsTl: ToList[Fields, Field[_ <: String, _]] 
  ): EntityFinder[N, Properties, PN, PT] = new EntityFinder(vertex, property) {

    override def findAll[PList <: HList, FList <: HList](properties: PList, xs: Seq[PT])(implicit 
      plub: LUBConstraint[PList, AnyProperty],
      rtr: RecordTRel[PList, Property, FList, Field],
      basis: BasisConstraint[PList, Properties],
      tl: ToList[PList, AnyProperty]
    ): Map[PT, Entity[FList]] = {
      val propertiesList = properties.toList[AnyProperty]
      xs.flatMap { key =>
        f(key).map { x =>
          // TODO Investigate adding LUBConstraint[FList, Field[_, _]] and cast at HList level
          key -> new Entity(
            x.fields.toList
              .filter(field => propertiesList.find(x => x.label == field.label).isDefined)
              .foldRight[HList](HNil)(shapeless.::(_, _))
          )(null).asInstanceOf[Entity[FList]]
        }
      }.toMap
    }
  }

  def fromMap[N <: String, Properties <: HList, Fields <: HList, PN <: String, PT](
    vertex: Vertex[N, Properties], 
    property: Property[PN, PT],
    xs: Map[PT, Entity[Fields]]
  )(implicit 
    propertiesLub: LUBConstraint[Properties, AnyProperty],
    pbasis: BasisConstraint[shapeless.::[Property[PN, PT], HNil], Properties],
    propertiesRtr: RecordTRel[Properties, Property, Fields, Field],
    fieldsTl: ToList[Fields, Field[_ <: String, _]] 
  ): EntityFinder[N, Properties, PN, PT] = fromFunction(vertex, property)(xs.get(_))
}
