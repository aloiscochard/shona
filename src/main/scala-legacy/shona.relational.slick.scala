package shona
package relational
package slick

import PartialFunction._
import scala.slick.lifted.AbstractTable
import scala.slick.ast._

import Graph._

trait ManagedSchema {

  object ManagedModel extends relational.Model {
    def relationships = managedTables.flatMap(_.relationships)
  }

  def managedTables: Seq[ManagedTable]
  
  trait ManagedTable { self: AbstractTable[_] =>
    import ManagedModel._

    val entity = new Entity {
      def name = self.tableName
      def properties = self.create_*.map(_.name).map(Property(_)).to[Seq]
    }

    // TODO Find solution to name properties/connections
    // TODO Support different kind of relationships, and what about FK with multiple columns?
    def relationships: Seq[Relationship] =
      self.foreignKeys.to[Seq].flatMap { fk =>
        condOpt {
          (
            fk.targetTable,
            fk.linearizedSourceColumns.collectFirst { case Select(_, x) => Property(x.name) },
            fk.linearizedTargetColumns.collectFirst { case Select(_, x) => Property(x.name) }
          )
        } {
          case (tt: ManagedTable, Some(sourceProperty), Some(targetProperty)) => 
            Relationship(
              Relationship.OneToMany,
              tt.entity.name,
              entity, 
              sourceProperty,
              entity.name,
              tt.entity,
              targetProperty
            )
        }
      }
  }

}
