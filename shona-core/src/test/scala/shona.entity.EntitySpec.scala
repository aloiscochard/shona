package shona
package entity

import org.specs2.mutable._

import record._

class EntitySpec extends Specification {

  val entity = Entity((Field[label("id")] ~ 1000L, Field[label("email")] ~ "alois.cochard@gmail.com"))

  "Shona Entity" should {
    "support view to access an entity fields" in {
      val view = View(entity)
      view.id === 1000L
      view.email === "alois.cochard@gmail.com"
      view.entity === entity
    }

    "not screw up the bytecode when the same view is requested multiple times" in {
      val view0 = View(entity)
      val view1 = View(entity)
      view0.entity === view1.entity
    }
  }
}
