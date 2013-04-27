package shona
package relational

import Graph._

// TODO Support ManyToMany relationship

trait Model {
  trait Entity extends Graph.Entity {
    import Connection._
    import Relationship._

    override lazy val connections = relationships.collect {
      case r if r.sourceEntity == this || r.targetEntity == this => connection(this, r)
    }
  }

  object Entity { def apply(n: String, ps: Seq[Graph.Property]) = new Entity { val name = n; val properties = ps } }

  trait Relationship {
    def tpe: Relationship.Type
    def sourceConnection: String
    def sourceEntity: Entity
    def sourceProperty: Property
    def targetConnection: String
    def targetEntity: Entity
    def targetProperty: Property
  }

  object Relationship {
    sealed trait Type
    //case object ManyToMany extends Type
    case object OneToMany extends Type
    case object OneToOne extends Type

    def apply(t: Type, sc: String, se: Entity, sp: Property, tc: String, te: Entity, tp: Property) = new Relationship { 
      val tpe = t
      val sourceConnection = sc; val sourceEntity = se; val sourceProperty = sp;
      val targetConnection = tc; val targetEntity = te; val targetProperty = tp
    }
  }

  trait Connection extends Graph.Connection {
    import Connection._

    def target = targetEntity
    def direction: Direction
    def relationship: Relationship

    // TODO Refactor for easier swapping (encapsulate connection/property/entity in a datastructure)
    def name = direction match { 
      case SourceToTarget => relationship.sourceConnection
      case TargetToSource => relationship.targetConnection
    }

    def tpe: Connection.Type = (direction, relationship.tpe) match {
      case (SourceToTarget, Relationship.OneToMany) => ToMany
      case _ => ToOne
    }

    def sourceProperty = direction match { 
      case SourceToTarget => relationship.sourceProperty
      case TargetToSource => relationship.targetProperty
    }

    def targetEntity = direction match { 
      case SourceToTarget => relationship.targetEntity
      case TargetToSource => relationship.sourceEntity
    }

    def targetProperty = direction match { 
      case SourceToTarget => relationship.targetProperty
      case TargetToSource => relationship.sourceProperty
    }
  }

  object Connection {
    sealed trait Type
    case object ToMany extends Type
    case object ToOne extends Type
    sealed trait Direction
    case object SourceToTarget extends Direction
    case object TargetToSource extends Direction
  }

  type Path = shona.Path

  object Path {
    def apply(entity: Entity)(expression: Expression.Tree): Option[Path] = path.Path(entity)(expression)
  }


  def relationships: Seq[Relationship]

  def connection(e: Entity, r: Relationship) = 
    new Connection { 
      val direction = if (r.sourceEntity == e) Connection.SourceToTarget else Connection.TargetToSource
      val relationship = r
    } 
}

// TODO Move to JSON package? Create a generic (non-json) version?
abstract class EntityLoader[M <: Model](model: M) {
  import org.json4s._
  import path._
  import model.Connection._

  def load(entity: M#Entity, properties: Seq[Property]): Seq[JObject]
  def find(entity: M#Entity, properties: Seq[Property], property: Property, values: Seq[JValue]): Seq[JObject]
  def findAll(entity: M#Entity, properties: Seq[Property], property: Property, value: Seq[JValue]): Seq[Seq[JObject]]
  def get(entity: M#Entity, property: Property)(x: JObject): JValue = x \ property.name

  def apply(entity: M#Entity)(path: M#Path): Seq[JObject] = {
    val segment :: tail = path
    val entities = load(entity, properties(entity, segment))

    // TODO Avoid 1 + N issue in recursive entity! (== flatten instead of recursive) ... finish impl
    entities
    /*
    connections(path).map { case (connection, path) =>
      val segment :: tail = path
      val values = entities.map(get(entity, connection.sourceProperty))
      (connection.tpe match {
        case ToOne =>
          find(connection.targetEntity, properties(connection.targetEntity, segment), connection.targetProperty, values)
        case ToMany => 
          findAll(connection.targetEntity, properties(connection.targetEntity, segment), connection.targetProperty, values)
            .map(_.toList)
            .map(JArray(_))
      }).map(connection.name -> _)
    }
    */
  }

  private def connections(path: Path): Seq[(M#Connection, Path)] = {
    def _connections(path: Path): Seq[(M#Connection, Path)] = path match {
      case View(fields) :: Nil => fields.flatMap(_connections(_))
      case Element(connection: M#Connection) :: tail => (connection -> tail) :: Nil
      case _ => Nil
    }
    _connections(path)
  }

  private def properties(entity: M#Entity, segment: Segment): Seq[Property] = {
    def _properties(segment: Segment): Seq[Property] = segment match {
      case View(fields) => fields.map({ case segment :: tail => segment }).flatMap(_properties(_))
      case Element(property: Property) => property :: Nil
      case Element(connection: M#Connection) => 
        if (connection.relationship.sourceEntity == entity) 
          connection.relationship.sourceProperty :: Nil
        else
          connection.relationship.targetProperty :: Nil
      case _ => Nil
    }
    _properties(segment)
  }
}

