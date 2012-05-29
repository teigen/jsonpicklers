package pickles

import net.liftweb.json.JsonAST._
import util.matching.Regex

// TODO, tryPickle from byteme

sealed trait Pickler[A, Json <: JValue, Like[X] <: Pickler[X, Json, Like]]{ self =>
  def tryPickle:A => Option[Json]
  def pickle: A => Json = tryPickle.andThen(_.get)
  def unpickle:Parser[A]

  def wrap[B](w:A => B)(u:B => A):Like[B]
  
  def ^^ [B](wrapper:Wrap[A, B]) = 
    wrap(wrapper.wrap)(wrapper.unwrap)
  
  def filter(predicate:A => Boolean, msg:String):Like[A]
  
  // TODO const, ~>, <~

  def apply(values:A*) = filter(values.contains, "expected one of " + values.mkString("(", ",", ")"))
  
  def optional :Like[Option[A]]

  def ? = optional
  
  def ? (orElse: => A):Like[A] = 
    self.?.getOrElse(orElse) 

  def <  (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.lt(a, rhs),   "expected value < "  + rhs)
  def <= (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.lteq(a, rhs), "expected value <= " + rhs)
  def >  (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.gt(a, rhs),   "expected value > "  + rhs)
  def >= (rhs:A)(implicit ordering:Ordering[A]) = filter(a => ordering.gteq(a, rhs), "expected value >= " + rhs)

  def getOrElse[B](orElse: => B)(implicit ev1:A => Option[B], ev2:Option[B] => A) = wrap(_.getOrElse(orElse))(Some(_))
}

trait Or[A, Json <: JValue, Like[X] <: Pickler[X, Json, Like]] extends Pickler[A, Json, Like]{
  def or[T >: A, B <: T](other:Pickler[B, Json, Like])(implicit ra:Reify[A], rb:Reify[B]):Like[T]
  def | [T >: A, B <: T](other:Pickler[B, Json, Like])(implicit ra:Reify[A], rb:Reify[B]):Like[T] = or[T, B](other)
}

object JsonValue {
  def apply[A](name:String)(u:PartialFunction[JValue, A])(p:A => JValue):JsonValue[A] = {
    def pickle(a: A) = p(a)
    def unpickle = Parser{ location => u.lift(location.json).map(v => Success(v, location)).getOrElse(Failure("expected " + name, location)) }
    JsonValue(unpickle, pickle)
  }

  def apply[A](unpickle:Parser[A], pickle:A => JValue) =
    new JsonValue[A](unpickle, a => Some(pickle(a)))
}

class JsonValue[A](val unpickle:Parser[A], val tryPickle:A => Option[JValue]) extends Pickler[A, JValue, JsonValue] with Or[A, JValue, JsonValue] { self =>

  def wrap[B](w: (A) => B)(u: (B) => A) = {
    def pickle(b: B) = self.pickle(u(b))
    def unpickle = self.unpickle.map(w)
    JsonValue(unpickle, pickle)
  }

  def filter(predicate: (A) => Boolean, msg: String) = {
    def pickle(a: A) = if(predicate(a)) self.pickle(a) else sys.error(msg)
    def unpickle = Parser{ location => self.unpickle(location).flatMap(v => if(predicate(v)) Success(v, location) else Failure(msg, location)) }
    JsonValue(unpickle, pickle)
  }

  def optional: JsonValue[Option[A]] = {
    def pickle(a: Option[A]) = a.map(self.pickle).getOrElse(JNull)
    def unpickle = Parser{ location =>
      location.json match {
        case JNull => Success(None, location)
        case _ => self.unpickle(location).map(Some(_))
      }
    }
    JsonValue(unpickle, pickle)
  }


  def or[T >: A, B <: T](other: Pickler[B, JValue, JsonValue])(implicit ra:Reify[A], rb:Reify[B]) = {
    def tryPickle(t: T) = (ra.reify(t).flatMap(self.tryPickle) orElse rb.reify(t).flatMap(other.tryPickle))
    def unpickle:Parser[T] = {
      val t:Parser[T] = self.unpickle
      t | other.unpickle
    }
    new JsonValue[T](unpickle, tryPickle)
  }

  def unpickle(json:JValue):Result[A] = unpickle(Root(json)) match {
    case Success(value, _) => Success(value, Root(json))
    case n => n
  }

  def * :JsonValue[List[A]] = {
    def pickle(a: List[A]) = JArray(a.map(self.pickle))
    def unpickle = Parser{ location => 
      location.json match {
        case JArray(values) => (0 until values.size).foldRight[Result[List[A]]](Success(Nil, location)){
          case (index, lst) => for{
            u    <- self.unpickle(location(index))
            tail <- lst
          } yield u :: tail
        }
        case _ => Failure("expected array", location)
      }
    }
    JsonValue(unpickle, pickle _)
  }

  def :: (name:String) = JsonProperty(name, this)
  
  def :: (selector:Selector): JsonValue[Map[String, A]] = {
    def unpickle = Parser{ location => 
      location.json match {
        case JObject(fields) =>
          fields.filter(field => selector.filter(field.name)).foldLeft[Result[Map[String, A]]](Success(Map.empty, location)){
            (map, field) => for{
              m <- map
              v <- self.unpickle(location(field.name))
            } yield m + (field.name -> v)
          }
        case _ => Failure("expected object", location)
      }
    }

    def pickle(a: Map[String, A]) = JObject(a.toList.map{ case (name, v) => JField(name, self.pickle(v)) })
    
    JsonValue(unpickle, pickle)
  }
}

object JsonProperty{
  implicit def asObject[A](field:JsonProperty[A]) =
    JsonObject(field.unpickle, field.pickle)
  
  implicit def asValue[A](field:JsonProperty[A]) =
    JsonValue(field.unpickle, field.pickle)  
}
case class JsonProperty[A](name:String, value:JsonValue[A]) extends Pickler[A, JObject, JsonProperty] { self =>
  
  def tryPickle = value.tryPickle(_).map{
    case JNothing => JObject(Nil)
    case something => JObject(List(JField(name, something)))
  }
  
  def unpickle = Parser{ location => value.unpickle(location(name)) }

  def optional = {
    def pickle(a: Option[A]) = a.map(value.pickle).getOrElse(JNothing)
    def unpickle = Parser{ location => 
      location.json match {
        case JNothing => Success(None, location)
        case _ => value.unpickle(location).map(Some(_))
      }
    }
    JsonProperty(name, JsonValue(unpickle, pickle))
  }

  def wrap[B](w: (A) => B)(u: (B) => A) = 
    JsonProperty(name, value.wrap(w)(u))

  def filter(predicate: (A) => Boolean, msg: String) = 
    JsonProperty(name, value.filter(predicate, msg))
}

object JsonObject {
  implicit def asValue[A](obj:JsonObject[A]) =
    JsonValue(obj.unpickle, obj.pickle)
  
  def apply[A](unpickle:Parser[A], pickle:A => JObject) =
    new JsonObject[A](unpickle, a => Some(pickle(a)))
}

class JsonObject[A](val unpickle:Parser[A], val tryPickle:A => Option[JObject]) extends Pickler[A, JObject, JsonObject] with Or[A, JObject, JsonObject]{ self =>

  def wrap[B](w: (A) => B)(u: (B) => A) = {
    def pickle(b: B) = self.pickle(u(b))
    def unpickle = self.unpickle.map(w)
    JsonObject(unpickle, pickle)
  }

  def filter(predicate: (A) => Boolean, msg: String) = {
    def pickle(a: A) = if(predicate(a)) self.pickle(a) else sys.error(msg)
    def unpickle = Parser{ location => self.unpickle(location).flatMap(v => if(predicate(v)) Success(v, location) else Failure(msg, location)) }
    JsonObject(unpickle, pickle)
  }

  def or[T >: A, B <: T](other: Pickler[B, JObject, JsonObject])(implicit ra:Reify[A], rb:Reify[B]) = {
    def tryPickle(t: T) = (ra.reify(t).flatMap(self.tryPickle) orElse rb.reify(t).flatMap(other.tryPickle))
    def unpickle:Parser[T] = {
      val t:Parser[T] = self.unpickle
      t | other.unpickle
    }
    new JsonObject[T](unpickle, tryPickle)
  }

  def optional = sys.error("TODOD: object.optional")

  def ~[B] (other:JsonObject[B]) = {
    
    val unpickle =   
      for{
        a <- self.unpickle
        b <- other.unpickle
      } yield new ~(a, b)

    def pickle(ab: A ~ B) = {
      val (a ~ b) = ab
      self.pickle(a) merge other.pickle(b)
    }

    JsonObject[A ~ B](unpickle, pickle)  
  }
}

trait Reify[A]{ self =>
  def reify(any:Any):Option[A]
  
  def | [T >: A, B <: T](b:Reify[B]):Reify[T] = new Reify[T]{
    def reify(any: Any) = {
      val ra:Option[T] = self.reify(any)
      ra orElse b.reify(any)
    } 
  }
}

object Reify extends ReifyAnyRef with ReifyAnyVal {
  
  def apply[A](pf:PartialFunction[Any, A]):Reify[A] = new Reify[A] {
    def reify(any: Any) = pf.lift(any)
  }
  
  def opt[A](pf:PartialFunction[Any, Option[A]]):Reify[A] = new Reify[A]{
    def reify(any: Any) = pf.lift(any).flatMap(identity)
  }

  implicit val NULL    = Reify{ case null => null }
  
  implicit def any:Reify[Any] = anyVal | NULL | anyRef[AnyRef]  
  
  implicit def left[A : Reify] = Reify.opt{
    case Left(value) => implicitly[Reify[A]].reify(value).map(Left(_))
  }
  
  implicit def right[A : Reify] = Reify.opt{
    case Right(value) => implicitly[Reify[A]].reify(value).map(Right(_))
  }

  implicit def either[A : Reify, B : Reify]:Reify[Either[A, B]] = left[A] | right[B]

  implicit def option[A : Reify] = Reify.opt{
    case Some(value) => implicitly[Reify[A]].reify(value).map(Some(_))
    case None        => None
  }
}

trait ReifyAnyRef {
  implicit def anyRef[A <: AnyRef : Manifest]:Reify[A] = new Reify[A]{
    val man = implicitly[Manifest[A]]

    def reify(any: Any) = any match {
      case ref:AnyRef =>
        val result = man >:> ClassManifest.singleType(ref)
        if (result) Some(ref.asInstanceOf[A]) else None
      case _ => None
    }
  }
}

trait ReifyAnyVal {
  implicit val boolean = Reify{ case b:Boolean => b }
  implicit val byte    = Reify{ case b:Byte => b }
  implicit val short   = Reify{ case s:Short => s }
  implicit val int     = Reify{ case i:Int => i }
  implicit val long    = Reify{ case l:Long => l }
  implicit val float   = Reify{ case f:Float => f }
  implicit val double  = Reify{ case d:Double => d }
  implicit val char    = Reify{ case c:Char => c }
  
  implicit val anyVal:Reify[AnyVal] = boolean | byte | short | int | int | long | float | double | char
}

case class Selector(filter:String => Boolean)

object Selector {
  
  val * = Selector(_ => true)
  
  implicit def filter(f:String => Boolean) = 
    Selector(f)
  
  implicit def regex(r:Regex) = 
    Selector(r.pattern.matcher(_).matches())
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