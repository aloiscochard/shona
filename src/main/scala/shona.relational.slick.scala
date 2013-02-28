package shona
package relational
package slick

import PartialFunction._
import scala.slick.lifted.AbstractTable
import scala.slick.lifted.Column
import scala.slick.ast._

import com.google.common.base.CaseFormat._

import Graph._

trait ManagedSchema {

  object ManagedModel extends relational.Model {
    def relationships = managedTables.flatMap(_.relationships)
  }

  case class ManagedColumn(columnName: String, propertyName: String) extends Property {
    def name = propertyName
  }

  object ManagedColumn {
    def apply(columnName: String): ManagedColumn = ManagedColumn(columnName, format(ManagedSchema.Column, columnName))
  }

  trait ManagedTable { self: AbstractTable[_] =>
    import ManagedModel._

    val entity = new Entity {
      def name = format(ManagedSchema.Table, self.tableName)
      def properties = self.create_*.map(_.name).map(ManagedColumn(_)).to[Seq]
    }

    // TODO Find solution to name properties/connections
    // TODO Support different kind of relationships, and what about FK with multiple columns?
    def relationships: Seq[Relationship] =
      self.foreignKeys.to[Seq].flatMap { fk =>
        condOpt {
          (
            fk.targetTable,
            fk.linearizedSourceColumns.collectFirst { case Select(_, x) => ManagedColumn(x.name) },
            fk.linearizedTargetColumns.collectFirst { case Select(_, x) => ManagedColumn(x.name) }
          )
        } {
          case (tt: ManagedTable, Some(sourceProperty), Some(targetProperty)) => 
            Relationship(
              Relationship.OneToMany,
              format(ManagedSchema.ConnectionToOne, tt.entity.name),
              entity, 
              sourceProperty,
              format(ManagedSchema.ConnectionToMany, entity.name),
              tt.entity,
              targetProperty
            )
        }
      }
  }

  def managedTables: Seq[ManagedTable]

  def formatter: PartialFunction[(ManagedSchema.Type, String), String] = {
    case (ManagedSchema.ConnectionToMany, label) => label
    case (ManagedSchema.ConnectionToOne, label) => label.dropRight(1)
    case (_, label) => UPPER_UNDERSCORE.to(LOWER_CAMEL, label)
  }

  private def format(tpe: ManagedSchema.Type, label: String) = formatter.lift(tpe -> label).getOrElse(label)
}

object ManagedSchema {
  sealed trait Type
  case object Column extends Type
  case object Table extends Type
  case object ConnectionToMany extends Type
  case object ConnectionToOne extends Type
}
