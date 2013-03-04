package jsonpicklers

import org.json4s.JsonAST._

/*
 * TODO, error messages on failing tryPickle ?
 */

object Picklers extends Picklers with FlattenTilde

trait Picklers {
  val  * = Selector.*

  val string  = JsonValue[String](Parsers.string, v => Some(JString(v)))
  val int     = JsonValue[Int](Parsers.int, v => Some(JInt(v)))
  val double  = JsonValue[Double](Parsers.double, v => Some(JDouble(v)))
  val bigint  = JsonValue[BigInt](Parsers.bigint, v => Some(JInt(v)))
  val boolean = JsonValue[Boolean](Parsers.boolean, v => Some(JBool(v)))
  val NULL    = new JsonValue[Null](Parsers.NULL, _ => Some(JNull)){
    def apply[B](value:B):JsonValue[B] = wrap(_ => value)(_ => null)
  }

  def array[A](value:JsonValue[A]) = value.*
  def wrap[A, B](w:A => B)(u:B => A) = Wrap(w, u)
  def option[A, Json <: JValue, Like[X] <: Pickler[X, Json, Like]](pickler:Pickler[A, Json, Like]):Like[Option[A]] = pickler.optional
  def select[A](filter:String => Boolean, value:JsonValue[A]) = Selector.filter(filter) :: value

  def either[A : Reify, B : Reify, Json <: JValue, Like[X] <: Or[X, Json, Like]](a:Pickler[A, Json, Like], b:Pickler[B, Json, Like]):Like[Either[A, B]] = {
    val left  = a.wrap(Left(_))(_.a)
    val right = b.wrap(Right(_))(_.b)
    left | right
  }

  def unique[A](values:JsonValue[List[A]]) = 
    values.filter(v => v.distinct == v, "expected all elements to be unique")

  implicit def propertyIsObject[A](prop:JsonProperty[A]) =
    JsonProperty.asObject(prop)
}

sealed trait Pickler[A, Json <: JValue, Like[X] <: Pickler[X, Json, Like]]{ self =>
  def tryPickle:A => Option[Json]
  def pickle: A => Json = tryPickle.andThen(_.get)
  def unpickle:Parser[A]

  def flatWrap[B](w:A => Location => Result[B])(u:B => Option[A]):Like[B]

  def trying[B](w:A => Location => Result[B])(u:B => Option[A]):Like[B] =
    flatWrap[B](a => l => try{ w(a)(l) } catch { case ex:Exception => Parsers.failure(ex.getMessage)(l) })(u)

  def wrap[B](w:A => B)(u:B => A):Like[B] =
    flatWrap[B](a => l => Success(w(a), l))(b => Some(u(b)))
  
  def ^^ [B](wrapper:Wrap[A, B]) = 
    wrap(wrapper.wrap)(wrapper.unwrap)
  
  def filter(predicate:A => Boolean, msg:String):Like[A] =
    flatWrap[A](a => l => if(predicate(a)) Success(a, l) else Failure(msg, l))(a => if (predicate(a)) Some(a) else None)
  
  // TODO const, ~>, <~

  def apply(values:A*) = filter(values.contains, "expected one of " + values.mkString("(", ",", ")"))
  
  def optional :Like[Option[A]]

  def ? = optional
  
  def ? (orElse: => A):Like[A] = 
    self.?.getOrElse(orElse)

  def ?? (orElse: A): Like[A] =
    self.?.getOrElseIgnoreDefault(orElse)

  def <  (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.lt(a, rhs),   "expected value < "  + rhs)
  def <= (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.lteq(a, rhs), "expected value <= " + rhs)
  def >  (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.gt(a, rhs),   "expected value > "  + rhs)
  def >= (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.gteq(a, rhs), "expected value >= " + rhs)

  def getOrElse[B](orElse: => B)(implicit ev1:A => Option[B], ev2:Option[B] => A) = wrap(_.getOrElse(orElse))(Some(_))
  def getOrElseIgnoreDefault[B](orElse: B)(implicit ev1:A => Option[B], ev2:Option[B] => A) = wrap(_.getOrElse(orElse))(u => if(orElse == u) None else Some(u))
}

trait Or[A, Json <: JValue, Like[X] <: Pickler[X, Json, Like]] extends Pickler[A, Json, Like]{
  def or[T >: A, B <: T](other:Pickler[B, Json, Like])(implicit ra:Reify[A], rb:Reify[B]):Like[T]
  def | [T >: A, B <: T](other:Pickler[B, Json, Like])(implicit ra:Reify[A], rb:Reify[B]):Like[T] = or[T, B](other)
}

case class JsonValue[A](unpickle:Parser[A], tryPickle:A => Option[JValue]) extends Pickler[A, JValue, JsonValue] with Or[A, JValue, JsonValue] { self =>

  def flatWrap[B](w: (A) => (Location) => Result[B])(u: (B) => Option[A]) =
    JsonValue[B](Parser{ location => unpickle(location).flatMap(w(_)(location))}, u(_).flatMap(tryPickle))

  def optional: JsonValue[Option[A]] = {
    def tryPickle(a: Option[A]) = a.map(self.tryPickle).getOrElse(Some(JNull))
    def unpickle = Parsers.NULL ^^^ None | self.unpickle.map(Some(_)) 
    JsonValue(unpickle, tryPickle)
  }

  def or[T >: A, B <: T](other: Pickler[B, JValue, JsonValue])(implicit ra:Reify[A], rb:Reify[B]) = {
    def tryPickle(t: T) = ra.reify(t).flatMap(self.tryPickle) orElse rb.reify(t).flatMap(other.tryPickle)
    def unpickle:Parser[T] = {
      val t:Parser[T] = self.unpickle
      t | other.unpickle
    }
    JsonValue[T](unpickle, tryPickle)
  }

  def unpickle(json:JValue):Result[A] = unpickle(Root(json)) match {
    case Success(value, _) => Success(value, Root(json))
    case n => n
  }

  def * :JsonValue[List[A]] = {
    def tryPickle(list:List[A]) = list.foldRight[Option[List[JValue]]](Some(Nil)){
      (a, acc) => 
        for{
          lst <- acc
          elem <- self.tryPickle(a)
        } yield elem :: lst
    }
    JsonValue(unpickle.*, tryPickle(_).map(JArray))
  }

  def :: (name:String) = JsonProperty(name, this)
  
  def :: (selector:Selector): JsonValue[Map[String, A]] = {
    def tryPickle(a: Map[String, A]) = a.toList.foldRight[Option[List[JField]]](Some(Nil)){
      case ((name, v), acc) => 
        for{
          list <- acc
          pickled <- self.tryPickle(v)
        } yield (name, pickled) :: list
    }
    JsonValue(selector :: unpickle, tryPickle(_).map(fields => JObject(fields)))
  }
}

object JsonProperty{
  implicit def asObject[A](field:JsonProperty[A]) =
    JsonObject[A](field.unpickle, field.tryPickle)

  implicit def asValue[A](field:JsonProperty[A]) =
    JsonValue[A](field.unpickle, field.tryPickle)
}

case class JsonProperty[A](name:String, value:JsonValue[A]) extends Pickler[A, JObject, JsonProperty] { self =>
  
  def tryPickle = value.tryPickle(_).map{
    case JNothing  => JObject(Nil)
    case something => JObject(List(JField(name, something)))
  }
  
  def unpickle = name :: value.unpickle

  def optional = {
    def tryPickle(a: Option[A]) = a.map(value.tryPickle).getOrElse(Some(JNothing))
    def unpickle = Parsers.nothing.map(_ => None) | value.unpickle.map(Some(_))
    JsonProperty(name, JsonValue[Option[A]](unpickle, tryPickle))
  }

  def flatWrap[B](w: (A) => (Location) => Result[B])(u: (B) => Option[A]) =
    JsonProperty(name, value.flatWrap(w)(u))
}

object JsonObject {
  implicit def asValue[A](obj:JsonObject[A]) =
    JsonValue[A](obj.unpickle, obj.tryPickle)
}

case class JsonObject[A](unpickle:Parser[A], tryPickle:A => Option[JObject]) extends Pickler[A, JObject, JsonObject] with Or[A, JObject, JsonObject]{ self =>

  def flatWrap[B](w: (A) => (Location) => Result[B])(u: (B) => Option[A]) =
    JsonObject[B](Parser{ location => unpickle(location).flatMap(w(_)(location))}, u(_).flatMap(tryPickle))

  def or[T >: A, B <: T](other: Pickler[B, JObject, JsonObject])(implicit ra:Reify[A], rb:Reify[B]) = {
    def tryPickle(t: T) = ra.reify(t).flatMap(self.tryPickle) orElse rb.reify(t).flatMap(other.tryPickle)
    def unpickle:Parser[T] = {
      val t:Parser[T] = self.unpickle
      t | other.unpickle
    }
    JsonObject[T](unpickle, tryPickle)
  }

  def optional = 
    JsonObject[Option[A]](unpickle.?, _.map(tryPickle).getOrElse(Some(JObject(Nil))))

  def ~[B] (other:JsonObject[B]) = {
    def tryPickle(a:A, b:B) = 
      for{
        pa <- self.tryPickle(a)
        pb <- other.tryPickle(b)
      } yield pa merge pb

    JsonObject[A ~ B](unpickle ~ other.unpickle, { case a ~ b => tryPickle(a, b) })
  }
}

case class ~[+A, +B](_1:A, _2:B){
  def ~[C](c:C) = new ~(this, c)
}

case class Wrap[A, B](wrap:A => B, unwrap:B => A){
  def ^^[C](wrapper:Wrap[B, C]) = 
    Wrap[A, C](wrap andThen wrapper.wrap, wrapper.unwrap andThen unwrap)
  
  def apply[Json <: JValue, Like[X] <: Pickler[X, Json, Like]](pickler:Pickler[A, Json, Like]) =
    pickler ^^ this
}