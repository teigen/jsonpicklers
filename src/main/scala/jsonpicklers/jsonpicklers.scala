package jsonpicklers

import annotation.implicitNotFound
import net.liftweb.json.JsonAST._

import util.matching.Regex

case class ~[+A, +B](_1:A, _2:B){
  def ~[C](c:C) = new ~(this, c)
}

@implicitNotFound("Don't know how to wrap ${A} in ${B}")
trait Wrap[A, B] { wrapper =>
  def wrap(value:A):B
  def unwrap(value:B):A

  def apply(self:JsonType[A])     = self.wrap(this)
  def apply(self:JsonObject[A])   = self.wrap(this)
  def apply(self:JsonProperty[A]) = self.wrap(this)
}

object Wrap{
  def apply[A, B](w:A => B)(u:B => A):Wrap[A, B] = new Wrap[A, B]{
    def wrap(value: A)   = w(value)
    def unwrap(value: B) = u(value)
  }
}

object JsonType{
  
  def apply[A](pickle:A => JValue, unpickle:(Location, JValue) => Result[A]):JsonType[A] = {
    val p = pickle
    val u = unpickle
    
    new JsonType[A]{
      def unpickle(location: Location, json: JValue) = u(location, json)
      def pickle(value: A) = p(value)
    }
  }
}

trait JsonType[A] { self =>

  def apply(values:A*) = filter(values.contains, "expected one of " + values.mkString("(", ",", ")"))

  def :: (name:String)                  = property(name, self)
  def :: (selector:JsonProperty.Filter) = properties(selector.filter, self)

  def * = array(this)
  
  def | [B >: A](co:CoVariantJsonType[B])(implicit reify:Reify[A]) = or(co) 
  def or[B >: A](co:CoVariantJsonType[B])(implicit reify:Reify[A]) = CoVariantJsonType.covariantJsonType(this) or co
  
  def filter(predicate:A => Boolean, msg:String) = 
    flatWrap[A](v => if(predicate(v)) pickle(v) else sys.error(msg))((value, location, json) => if(predicate(value)) Success(value, location, json) else Failure(msg, location, json))
  
  def flatWrap[B](pickle:(B) => JValue)(unpickle:(A, Location, JValue) => Result[B]):JsonType[B] = 
    JsonType[B](pickle, (location, json) => self.unpickle(location, json).flatMap(a => unpickle(a, location, json)))
  
  def <  (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.lt(lhs, rhs),    "expected < " + rhs)
  def <= (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.lteq(lhs, rhs),  "expected <= " + rhs)
  def >  (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.gt(lhs, rhs),    "expected > " + rhs)
  def >= (rhs:A)(implicit ordering:Ordering[A])   = filter(lhs => ordering.gteq(lhs, rhs),  "expected >= " + rhs)
  def equiv(rhs:A)(implicit ordering:Ordering[A]) = filter(lhs => ordering.equiv(lhs, rhs), "expected equiv " + rhs)   

  def wrap[B](implicit wrapper:Wrap[A, B]):JsonType[B] = wrap(wrapper.wrap, wrapper.unwrap)
  def wrap[B](w:A => B, u:B => A) = flatWrap[B](b => pickle(u(b)))((a, location, json) => Success(w(a), location, json))

  def unpickle(json:JValue):Result[A] = unpickle(Root, json) match {
    case Success(value, _, _) => Success(value, Root, json)
    case f => f
  }

  def unpickle(location:Location, json:JValue):Result[A]
  def pickle(value:A):JValue
}

object JsonObject {
  def apply[A](pickle:A => JObject, unpickle:(Location, JValue) => Result[A]):JsonObject[A] = {
    val p = pickle
    val u = unpickle
    new JsonObject[A]{
      def pickle(value: A) = p(value)
      def unpickle(location: Location, json: JValue) = u(location, json)
    }
  }
  
  def filter[A](self:JsonObject[A], predicate:A => Boolean, msg:String) = JsonObject[A](a => if(predicate(a)) self.pickle(a) else sys.error("filter " + a), self.unpickle(_, _).filter(predicate, msg))
}

trait JsonObject[A] extends JsonType[A] { self =>

  def flatWrap[B](pickle:(B) => JObject)(unpickle:(A, Location, JValue) => Result[B]):JsonObject[B] =
    JsonObject[B](pickle, (location, json) => self.unpickle(location, json).flatMap(a => unpickle(a, location, json)))
  
  def ~[B](other:JsonObject[B]) = this.flatWrap[A ~ B]((ab:A~B) => JObject(pickle(ab._1).obj ++ other.pickle(ab._2).obj))((a:A, location:Location, json:JValue) => other.unpickle(location, json).map(b => new ~(a, b)))

  override def filter(predicate:A => Boolean, msg:String) = JsonObject.filter(this, predicate, msg)
  
  def | [B >: A](co:CoVariantJsonObject[B])(implicit reify:Reify[A])  = or(co)
  def or[B >: A](co:CoVariantJsonObject[B])(implicit reify:Reify[A]) = CoVariantJsonObject.covariantJsonObject(this) or co

  override def wrap[B](implicit wrapper:Wrap[A, B]):JsonObject[B] = wrap(wrapper.wrap, wrapper.unwrap)
  override def wrap[B](w:A => B, u:B => A) = JsonObject[B](b => pickle(u(b)), unpickle(_, _).map(w))
  
  override def pickle(value:A):JObject
}

object JsonProperty {
  
  case class Filter(filter:JField => Boolean)
  
  val all = Filter(_ => true)

  implicit def regex(r:Regex) = Filter{ case JField(name, _) => r.pattern.matcher(name).matches() }
}

case class JsonProperty[A](name:String, jsonType:JsonType[A]) extends JsonObject[A]{
  def unpickle(location: Location, json: JValue) = jsonType.unpickle(location, json \ name)
  
  override def pickle(value: A) = jsonType.pickle(value) match {
    case JNothing => JObject(Nil)
    case something => JObject(List(JField(name, something)))
  }
  
  def ? = option(this)
  
  def ? (other: => A):JsonProperty[A] = ?.getOrElse(other)

  def getOrElse[B](other:B)(implicit ev1:A <:< Option[B], ev2: Option[B] <:< A) =
    wrap[B]((a:A) => a.getOrElse(other), (b:B) => Some(b):A)

  override def wrap[B](implicit wrapper:Wrap[A, B]) = wrap(wrapper.wrap, wrapper.unwrap)
  override def wrap[B](w:A => B, u:B => A):JsonProperty[B] = JsonProperty[B](name, jsonType.wrap(w, u))

}


