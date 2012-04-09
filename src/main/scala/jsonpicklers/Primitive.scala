package jsonpicklers

import net.liftweb.json.JsonAST._

object Primitive {
  def primitive[A](name:String, p:A => JValue)(u:PartialFunction[(Location, JValue), Result[A]]) =
    JsonType[A](p, (location, json) => u.lift((location, json)).getOrElse(Failure("expected " + name, location, json)))
  
  val string = primitive[String]("string", JString(_)){
    case (location, json@JString(s)) => Success(s, location, json)
  }

  val boolean = primitive[Boolean]("boolean", JBool(_)){
    case (location, json@JBool(b)) => Success(b, location, json)
  }

  val double = primitive[Double]("double", JDouble(_)){
    case (location, json@JDouble(d)) => Success(d, location, json)
  }

  val integer = primitive[Int]("int", i => JInt(BigInt(i))){
    case (location, json@JInt(big)) if big.isValidInt => Success(big.intValue(), location, json)
  }

  val bigint = primitive[BigInt]("bigint", JInt(_)){
    case (location, json@JInt(i)) => Success(i, location, json)
  }

  val NULL = primitive[Null]("null", _ => JNull){
    case (location, json@JNull) => Success(null, location, json)
  }

  def array[A](a:JsonType[A]) = primitive[List[A]]("array", value => JArray(value map a.pickle)){
    case (location, json@JArray(arr)) => arr.zipWithIndex.map{ case (j, i) => a.unpickle(location(i), j)}.foldRight[Result[List[A]]](Success(Nil, location, json)){
      (e, acc) => for {
        elem     <- e
        elements <- acc
      } yield elem :: elements
    }
  }
  
  def option[A](self:JsonProperty[A]) = JsonProperty[Option[A]](self.name, JsonType[Option[A]](_.map(self.jsonType.pickle).getOrElse(JNothing), {
    case (location, json@JNothing) => Success(None, location, json)
    case (location, json) => self.jsonType.unpickle(location, json).map(Some(_))
  }))

//  def option[A](self:JsonProperty[A]):JsonProperty[Option[A]] = new JsonProperty[Option[A]]{
//    def field      = self.field
//
//    def unpickle(location:Location, json: JValue) = (json \ field) match {
//      case JNothing => Success(None, location, json)
//      case _        => self.unpickle(location, json).map(Some(_))
//    }
//
//    def pickle(value: Option[A]) = value.map(self.pickle).getOrElse(JObject(Nil))
//  }
  
}

object Combinators {

//  def sequence[A, B](self:JsonObject[A], other:JsonObject[B]):JsonObject[A ~ B] = new JsonObject[A ~ B]{
//    def unpickle(location:Location, json: JValue) = for {
//      a <- self.unpickle(location, json)
//      b <- other.unpickle(location, json)
//    } yield new ~(a, b)
//
//    def pickle(ab: A ~ B) = {
//      val (a ~ b) = ab
//      val JObject(aFields) = self.pickle(a)
//      val JObject(bFields) = other.pickle(b)
//      JObject(aFields ++ bFields)
//    }
//  }

  def property[A](name:String, self:JsonType[A]) = JsonProperty(name, self)

  def properties[A](predicate:JField => Boolean, self:JsonType[A]):JsonType[Map[String, A]] = new JsonType[Map[String, A]]{
    def unpickle(location: Location, json: JValue) = {
      val fields = json match {
        case JObject(f) => f
        case _ => Nil
      }
      val result = fields.filter(predicate).foldRight[Result[List[(String, A)]]](Success(Nil, location, json)) {
        case (JField(name, value), acc) => for {
          r <- self.unpickle(location(name), value)
          t <- acc
        } yield (name, r) :: t
      }
      result.map(_.toMap)
    }

    def pickle(value: Map[String, A]) = {
      val fields = value.toList.map{ case (k,v) => JField(k, self.pickle(v)) }
      JObject(fields.filter(predicate))
    }
  }
}
