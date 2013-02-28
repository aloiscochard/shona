package shona
package relational
package slick

import org.specs2.mutable._

class ShonaRelationalSlickSpec extends Specification {

  "ShonaRelationalSlick" should {
    "work" in {
      import path._

      val schema = new Schema(scala.slick.driver.H2Driver)

      schema.Suppliers.entity.properties
      println(schema.Coffees.relationships)

      def find(table: schema.ManagedTable)(expression: String) =
        Expression(expression).right.toOption.flatMap(Path(table.entity))
      
      println {
        find(schema.Suppliers)("(name, city, coffees(name, price, supplier.name))")
      }

      true === true
    }
  }

}
import scala.slick.driver.ExtendedProfile
class Schema(val driver: ExtendedProfile) extends ManagedSchema {
  import driver._

  val managedTables = Seq(Suppliers, Coffees)

  object Suppliers extends Table[(Int, String, String, String, String, String)]("SUPPLIERS") with ManagedTable {
    def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
    def name = column[String]("NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    def * = id ~ name ~ street ~ city ~ state ~ zip
  }

  object Coffees extends Table[(String, Int, Double, Int, Int)]("COFFEES") with ManagedTable {
    def name = column[String]("NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = name ~ supID ~ price ~ sales ~ total
    def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
  }
}

