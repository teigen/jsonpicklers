package jsonpicklers

import net.liftweb.json.JsonAST._

import util.matching.Regex

case class ~[+A, +B](_1:A, _2:B){
  def ~[C](c:C) = new ~(this, c)
}

trait Optional[A, Like[_]]{    
  def ? : Like[Option[A]] with JsonLike[Option[A], Like]
  def ? (other: => A):Like[A] = ?.getOrElse(other)
}

sealed trait JsonLike[A, Like[_]] { 

  def flatWrap[B](w:(A, Location, JValue) => Result[B])(u:B => A):Like[B]    
  
  def filter(predicate:A => Boolean, msg:String):Like[A] =
    flatWrap((a, location, json) => if(predicate(a)) Success(a, location, json) else Failure(msg, location, json))(a => if(predicate(a)) a else sys.error(msg))

  def as[B](implicit wrapper: Wrap[A, B]):Like[B] = 
    wrap(wrapper.wrap)(wrapper.unwrap)
  
  def wrap[B](w: (A) => B)(u: (B) => A):Like[B] = 
    flatWrap((a, location, json) => Success(w(a), location, json))(u)

  def <  (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.lt(lhs, rhs),    "expected < " + rhs)
  def <= (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.lteq(lhs, rhs),  "expected <= " + rhs)
  def >  (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.gt(lhs, rhs),    "expected > " + rhs)
  def >= (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.gteq(lhs, rhs),  "expected >= " + rhs)
  def equiv(rhs:A)(implicit ordering:Ordering[A]) = filter(lhs => ordering.equiv(lhs, rhs), "expected equiv " + rhs)

  def apply(values:A*) = filter(values.contains, "expected one of " + values.mkString("(", ",", ")"))

  def getOrElse[B](other:B)(implicit ev1:A <:< Option[B], ev2: Option[B] <:< A) =
    wrap(_.getOrElse(other))(Some(_))  
}

trait JsonTypeLike[A]{
  def asType:JsonType[A]

  def :: (name:String)                  = property(name, this)
  def :: (selector:JsonProperty.Filter) = properties(selector.filter, this)
  
  def unpickle(json:JValue):Result[A] = asType.unpickle(Root, json) match {
    case Success(value, _, _) => Success(value, Root, json)
    case f => f
  }

  def * = array(this)
}

object JsonType extends Primitives

case class JsonType[A](pickle:A => JValue, unpickle:(Location, JValue) => Result[A]) extends JsonLike[A, JsonType] with JsonTypeLike[A] with Optional[A, JsonType]{ self =>
  
  def asType = this
  
  def flatWrap[B](w:(A, Location, JValue) => Result[B])(u:B => A):JsonType[B] = 
    JsonType[B](u andThen pickle, (location, json) => unpickle(location, json).flatMap(a => w(a, location, json)))

  def ? = JsonType(_.map(self.pickle).getOrElse(JNull), (location, json) => self.unpickle(location, json).map(Some(_)) orElse NULL.unpickle(location, json).map(_ => None))

  def | [B >: A](co:CoVariantJsonType[B])(implicit reify:Reify[A]) = or(co) 
  def or[B >: A](co:CoVariantJsonType[B])(implicit reify:Reify[A]) = CoVariantJsonType.covariantJsonType(this) or co  
}

trait JsonObjectLike[A] {
  def asObject:JsonObject[A]
  
  def ~[B](o:JsonObjectLike[B]) = {
    val self = asObject
    val other = o.asObject
    JsonObject[A ~ B]({ case a ~ b => JObject(self.pickle(a).obj ++ other.pickle(b).obj) }, (location, json) => self.unpickle(location, json).flatMap(a => other.unpickle(location, json).map(b => new ~(a, b))))
  }

  def | [B >: A](co:CoVariantJsonObject[B])(implicit reify:Reify[A])  = or(co)
  def or[B >: A](co:CoVariantJsonObject[B])(implicit reify:Reify[A]) = CoVariantJsonObject.covariantJsonObject(asObject) or co
}

case class JsonObject[A](pickle:A => JObject, unpickle:(Location, JValue) => Result[A]) extends JsonLike[A, JsonObject] with JsonTypeLike[A] with JsonObjectLike[A] with Optional[A, JsonType]{ self =>

  def asType = JsonType(pickle, unpickle)
  def asObject = this
  def ? = asType.?

  def flatWrap[B](w:(A, Location, JValue) => Result[B])(u:B => A):JsonObject[B] =
    JsonObject[B](u andThen pickle, (location, json) => unpickle(location, json).flatMap(a => w(a, location, json)))    
}

object JsonProperty {
  
  case class Filter(filter:JField => Boolean)
  
  val all = Filter(_ => true)

  implicit def regex(r:Regex) = Filter{ case JField(name, _) => r.pattern.matcher(name).matches() }

  def properties[A](predicate:JField => Boolean, self:JsonType[A]):JsonType[Map[String, A]] = { 
    
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
    
    JsonType(pickle, unpickle)
  }
}

case class JsonProperty[A](name:String, jsonType:JsonType[A]) extends JsonLike[A, JsonProperty] with JsonTypeLike[A] with JsonObjectLike[A] with Optional[A, JsonProperty]{

  def pickle(value: A) = jsonType.pickle(value) match {
    case JNothing => JObject(Nil)
    case something => JObject(List(JField(name, something)))
  }

  def unpickle(location: Location, json: JValue) = jsonType.unpickle(location(name), json \ name)

  def asType   = JsonType(pickle, unpickle)
  def asObject = JsonObject(pickle, unpickle)

  def flatWrap[B](w:(A, Location, JValue) => Result[B])(u:B => A) =
    JsonProperty[B](name, jsonType.flatWrap(w)(u))

  def ? = JsonProperty[Option[A]](name, JsonType[Option[A]](_.map(jsonType.pickle).getOrElse(JNothing), {
    case (location, json@JNothing) => Success(None, location, json)
    case (location, json) => jsonType.unpickle(location, json).map(Some(_))
  }))
}


