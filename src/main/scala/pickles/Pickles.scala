package pickles

import net.liftweb.json.JsonAST._
import util.matching.Regex

sealed trait Location {
  def json:JValue
  
  override def toString = this match {
    case Root(_) => ""
    case ArrayLocation(_, index, parent) => parent.toString + "["+index+"]"
    case FieldLocation(_, name, Root(_)) => name
    case FieldLocation(_, name, parent)  => parent.toString+"."+name
  }
  def apply(name:String) = FieldLocation(json \ name, name, this)
  def apply(index:Int)   = ArrayLocation(json(index), index, this)
}

case class Root(json:JValue) extends Location
case class FieldLocation(json:JValue, name:String, parent:Location) extends Location
case class ArrayLocation(json:JValue, index:Int, parent:Location) extends Location

trait Result[+A]{
  def map[B](f:A => B) = this match {
    case Success(value, location) => Success(f(value), location)
    case f:Failure                => f
  }  
  def flatMap[B](f:A => Result[B]) = this match {
    case Success(value, location) => f(value)
    case f:Failure                => f
  }
  def orElse[B >: A](other: => Result[B]) = this match {
    case f:Failure => other
    case n         => n
  }
  
  def isFailure = this match {
    case _:Failure => true
    case _         => false
  }
  
  def isSuccess = !isFailure
}
case class Success[+A](value:A, location:Location) extends Result[A]
case class Failure(msg:String, location:Location) extends Result[Nothing]{
  override def toString = "Failure("+location+", "+msg + ", " + location.json+")"
}

case class ~[+A, +B](_1:A, _2:B){
  def ~[C](c:C) = new ~(this, c)
}

trait Pickler[A, Pickle <: JValue]{
  def pickle(a:A):Pickle
  def unpickle(location:Location):Result[A]
}

trait Optional[Like[_]]{
  def optional[A](like:Like[A]):Like[Option[A]]
}

object Optional {
  def apply[Like[_] : Optional] = implicitly[Optional[Like]]
  
  implicit val jsonValue = new Optional[JsonValue]{
    def optional[A](like: JsonValue[A]) = new JsonValue[Option[A]]{
      def pickle(a: Option[A]) = a.map(like.pickle).getOrElse(JNull)
      def unpickle(location: Location) = location.json match {
        case JNull => Success(None, location)
        case _ => like.unpickle(location).map(Some(_)) 
      }
    }
  }
  
  implicit val jsonProperty = new Optional[JsonProperty]{
    def optional[A](like: JsonProperty[A]) = JsonProperty(like.name, new JsonValue[Option[A]]{
      def pickle(a: Option[A]) = a.map(like.value.pickle).getOrElse(JNothing)
      def unpickle(location: Location) = location.json match {
        case JNothing => Success(None, location)
        case _ => like.value.unpickle(location).map(Some(_))
      }
    })
  }
}

trait Or[Like[_]]{
  def or[T, A <: T : Reify, B <: T : Reify](a:Like[A], b:Like[B]):Like[T]
}

object Or {
  def apply[Like[_] : Or] = implicitly[Or[Like]]
  
  implicit val jsonValue = new Or[JsonValue]{
    def or[T, A <: T : Reify, B <: T : Reify](a:JsonValue[A], b:JsonValue[B]):JsonValue[T] = new JsonValue[T]{
      def pickle(t: T) = (implicitly[Reify[A]].reify(t).map(a.pickle) orElse implicitly[Reify[B]].reify(t).map(b.pickle)).get
      def unpickle(location: Location) = a.unpickle(location) orElse b.unpickle(location)
    }
  }
  
  implicit val jsonObject = new Or[JsonObject]{
    def or[T, A <: T : Reify, B <: T : Reify](a:JsonObject[A], b:JsonObject[B]):JsonObject[T] = new JsonObject[T]{
      def pickle(t: T) = (implicitly[Reify[A]].reify(t).map(a.pickle) orElse implicitly[Reify[B]].reify(t).map(b.pickle)).get
      def unpickle(location: Location) = a.unpickle(location) orElse b.unpickle(location)
    }
  }
}

object JsonValue {
  def apply[A](name:String)(u:PartialFunction[JValue, A])(p:A => JValue):JsonValue[A] = new JsonValue[A]{
    def pickle(a: A) = p(a)
    def unpickle(location:Location) = u.lift(location.json).map(v => Success(v, location)).getOrElse(Failure("expected " + name, location))
  }
}

trait JsonValue[A] extends Pickler[A, JValue] with Syntax[A, JsonValue] { self =>
  def unpickle(json:JValue):Result[A] = unpickle(Root(json)) match {
    case Success(value, _) => Success(value, Root(json))
    case n => n
  }

  def * = new JsonValue[List[A]] {
    def pickle(a: List[A]) = JArray(a.map(self.pickle))
    def unpickle(location:Location) = location.json match {
      case JArray(values) => (0 until values.size).foldRight[Result[List[A]]](Success(Nil, location)){
        case (index, lst) => for{
          u    <- self.unpickle(location(index))
          tail <- lst
        } yield u :: tail
      }
      case _ => Failure("expected array", location)
    }
  }

  def :: (name:String) = JsonProperty(name, this)
  
  def :: (selector:Selector) = new JsonValue[Map[String, A]] {
    def unpickle(location:Location) = location.json match {
      case JObject(fields) =>
        fields.filter(field => selector.filter(field.name)).foldLeft[Result[Map[String, A]]](Success(Map.empty, location)){
          (map, field) => for{
            m <- map
            v <- self.unpickle(location(field.name))
          } yield m + (field.name -> v)
        }
      case _ => Failure("expected object", location)
    }

    def pickle(a: Map[String, A]) = JObject(a.toList.map{ case (name, v) => JField(name, self.pickle(v)) })
  }
}

object JsonProperty{
  implicit def asObject[A](field:JsonProperty[A]) = new JsonObject[A]{
    def pickle(a: A) = field.pickle(a)
    def unpickle(location:Location) = field.unpickle(location)
  }
  implicit def asValue[A](field:JsonProperty[A]):JsonValue[A] = new JsonValue[A]{
    def pickle(a: A) = field.pickle(a)
    def unpickle(location: Location) = field.unpickle(location)
  }
}
case class JsonProperty[A](name:String, value:JsonValue[A]) extends Pickler[A, JObject] with Syntax[A, JsonProperty] { self =>
  def pickle(a: A) = value.pickle(a) match {
    case JNothing => JObject(Nil)
    case something => JObject(List(JField(name, something)))
  }
  def unpickle(location:Location) = value.unpickle(location(name))
}
object JsonObject {
  implicit def asValue[A](obj:JsonObject[A]) = new JsonValue[A]{
    def pickle(a: A) = obj.pickle(a)
    def unpickle(location:Location) = obj.unpickle(location)
  }   
}
trait JsonObject[A] extends Pickler[A, JObject] with Syntax[A, JsonObject] { self =>
  def ~[B] (other:JsonObject[B]) = new JsonObject[A ~ B] {
    def unpickle(location:Location) = for{
      a <- self.unpickle(location)
      b <- other.unpickle(location)
    } yield new ~(a, b)

    def pickle(ab: A ~ B) = {
      val (a ~ b) = ab
      self.pickle(a) merge other.pickle(b)
    }
  }
}

trait Filter[Like[_]]{
  def filter[A](like:Like[A])(predicate:A => Boolean, msg: => String):Like[A]
}

object Filter {
  implicit val jsonValue = new Filter[JsonValue]{
    def filter[A](self: JsonValue[A])(predicate: (A) => Boolean, msg: => String) = new JsonValue[A]{
      def pickle(a: A) = if(predicate(a)) self.pickle(a) else sys.error(msg)
      def unpickle(location: Location) = self.unpickle(location).flatMap(v => if(predicate(v)) Success(v, location) else Failure(msg, location))
    }
  }
  
  implicit val jsonProperty = new Filter[JsonProperty]{
    def filter[A](like: JsonProperty[A])(predicate: (A) => Boolean, msg: => String) = JsonProperty(like.name, like.value.filter(predicate, msg))
  }
  
  implicit val jsonObject = new Filter[JsonObject]{
    def filter[A](self: JsonObject[A])(predicate: (A) => Boolean, msg: => String) = new JsonObject[A]{
      def pickle(a: A) = if(predicate(a)) self.pickle(a) else sys.error(msg)
      def unpickle(location: Location) = self.unpickle(location).flatMap(v => if(predicate(v)) Success(v, location) else Failure(msg, location))
    }
  }
}

trait Reify[A]{
  def reify(any:Any):Option[A]
}

object Reify{
  
  def apply[A](pf:PartialFunction[Any, A]):Reify[A] = new Reify[A] {
    def reify(any: Any) = pf.lift(any)
  }
  
  def option[A](pf:PartialFunction[Any, Option[A]]):Reify[A] = new Reify[A]{
    def reify(any: Any) = pf.lift(any).flatMap(identity)
  }
  
  implicit val string  = Reify{ case s:String => s }
  implicit val int     = Reify{ case i:Int => i }
  implicit val boolean = Reify{ case b:Boolean => b}
  implicit val NULL    = Reify{ case null => null }
  implicit val bigint  = Reify{ case b:BigInt => b }
  
  implicit def either[A : Reify, B : Reify] = Reify.option{
    case Left(value)  => implicitly[Reify[A]].reify(value).map(Left(_))
    case Right(value) => implicitly[Reify[B]].reify(value).map(Right(_))
  }
  
  implicit def left[A : Reify] = Reify.option{
    case Left(value) => implicitly[Reify[A]].reify(value).map(Left(_))
  }
  
  implicit def right[A : Reify] = Reify.option{
    case Right(value) => implicitly[Reify[A]].reify(value).map(Right(_))
  }
}

trait Selector{
  def filter(name:String):Boolean
}

object Selector {
  
  val * = new Selector{ def filter(name:String) = true }
  
  implicit def filter(f:String => Boolean) = new Selector{
    def filter(name:String) = f(name)
  }
  
  implicit def regex(r:Regex) = new Selector{
    def filter(name: String) = r.pattern.matcher(name).matches()
  }
}

trait Wrap[Like[_]]{
  def wrap[A, B](like:Like[A])(w:A => B)(u:B => A):Like[B]
}

object Wrap {
  def apply[Like[_] : Wrap] = implicitly[Wrap[Like]]
  
  implicit val jsonValue = new Wrap[JsonValue]{
    def wrap[A, B](like: JsonValue[A])(w: (A) => B)(u: (B) => A) = new JsonValue[B]{
      def pickle(b: B) = like.pickle(u(b))
      def unpickle(location: Location) = like.unpickle(location).map(w)
    }
  }
  
  implicit val jsonObject = new Wrap[JsonObject]{
    def wrap[A, B](like: JsonObject[A])(w: (A) => B)(u: (B) => A) = new JsonObject[B]{
      def pickle(b: B) = like.pickle(u(b))
      def unpickle(location: Location) = like.unpickle(location).map(w)
    }
  }
  
  implicit val jsonProperty = new Wrap[JsonProperty]{
    def wrap[A, B](like: JsonProperty[A])(w: (A) => B)(u: (B) => A) = JsonProperty(like.name, like.value.wrap(w)(u))
  }
}

trait Syntax[A, Like[_]]{
  this:Like[A] =>
  
  def wrap[B](w:A => B)(u:B => A)(implicit wrap:Wrap[Like]):Like[B] = pickles.wrap(w)(u)(this)
  def filter(predicate:A => Boolean, msg: => String)(implicit f:Filter[Like]):Like[A] = f.filter(this)(predicate, msg) 

  def apply(values:A*)(implicit ev:Filter[Like]) = filter(values.contains, "expected one of " + values.mkString("(", ",", ")"))
  
  def ? (implicit optional:Optional[Like]) = pickles.option(this)
  def ? (orElse: => A)(implicit optional:Optional[Like], wrapper:Wrap[Like]) = pickles.getOrElse(this, orElse)

  //ordering
  def <  (rhs:A)(implicit ordering:Ordering[A], f:Filter[Like]) = filter(a => ordering.lt(a, rhs),   "expected value < "  + rhs)
  def <= (rhs:A)(implicit ordering:Ordering[A], f:Filter[Like]) = filter(a => ordering.lteq(a, rhs), "expected value <= " + rhs)
  def >  (rhs:A)(implicit ordering:Ordering[A], f:Filter[Like]) = filter(a => ordering.gt(a, rhs),   "expected value > "  + rhs)
  def >= (rhs:A)(implicit ordering:Ordering[A], f:Filter[Like]) = filter(a => ordering.gteq(a, rhs), "expected value >= " + rhs)
  
  def getOrElse[B](orElse: => B)(implicit ev0:Wrap[Like], ev1:A => Option[B], ev2:Option[B] => A) = wrap(_.getOrElse(orElse))(Some(_))

  def | [T >: A, B <: T](other:Like[B])(implicit or:Or[Like], ra:Reify[A], rb:Reify[B]):Like[T] = pickles.or(this, other)
}

trait TildeSyntax {
  class Tilde[A](a:A){
    def ~[B](b:B) = new ~(a, b)
  }
  implicit def tilde[A](a:A) = new Tilde[A](a)
}
  
trait FlattenTilde {
  import tilde._
  
  implicit def flatten2[A1,A2,R](f:(A1,A2) => R) = (p: A1~A2) => p match { case (a1~a2) => f(a1,a2) }
  implicit def tilde2[A1,A2](f:((A1,A2))) = f match { case (a1,a2) => a1~a2}
  implicit def flatten3[A1,A2,A3,R](f:(A1,A2,A3) => R) = (p: A1~A2~A3) => p match { case (a1~a2~a3) => f(a1,a2,a3) }
  implicit def tilde3[A1,A2,A3](f:((A1,A2,A3))) = f match { case (a1,a2,a3) => a1~a2~a3}
  implicit def flatten4[A1,A2,A3,A4,R](f:(A1,A2,A3,A4) => R) = (p: A1~A2~A3~A4) => p match { case (a1~a2~a3~a4) => f(a1,a2,a3,a4) }
  implicit def tilde4[A1,A2,A3,A4](f:((A1,A2,A3,A4))) = f match { case (a1,a2,a3,a4) => a1~a2~a3~a4}
  implicit def flatten5[A1,A2,A3,A4,A5,R](f:(A1,A2,A3,A4,A5) => R) = (p: A1~A2~A3~A4~A5) => p match { case (a1~a2~a3~a4~a5) => f(a1,a2,a3,a4,a5) }
  implicit def tilde5[A1,A2,A3,A4,A5](f:((A1,A2,A3,A4,A5))) = f match { case (a1,a2,a3,a4,a5) => a1~a2~a3~a4~a5}
  implicit def flatten6[A1,A2,A3,A4,A5,A6,R](f:(A1,A2,A3,A4,A5,A6) => R) = (p: A1~A2~A3~A4~A5~A6) => p match { case (a1~a2~a3~a4~a5~a6) => f(a1,a2,a3,a4,a5,a6) }
  implicit def tilde6[A1,A2,A3,A4,A5,A6](f:((A1,A2,A3,A4,A5,A6))) = f match { case (a1,a2,a3,a4,a5,a6) => a1~a2~a3~a4~a5~a6}
  implicit def flatten7[A1,A2,A3,A4,A5,A6,A7,R](f:(A1,A2,A3,A4,A5,A6,A7) => R) = (p: A1~A2~A3~A4~A5~A6~A7) => p match { case (a1~a2~a3~a4~a5~a6~a7) => f(a1,a2,a3,a4,a5,a6,a7) }
  implicit def tilde7[A1,A2,A3,A4,A5,A6,A7](f:((A1,A2,A3,A4,A5,A6,A7))) = f match { case (a1,a2,a3,a4,a5,a6,a7) => a1~a2~a3~a4~a5~a6~a7}
  implicit def flatten8[A1,A2,A3,A4,A5,A6,A7,A8,R](f:(A1,A2,A3,A4,A5,A6,A7,A8) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8) => f(a1,a2,a3,a4,a5,a6,a7,a8) }
  implicit def tilde8[A1,A2,A3,A4,A5,A6,A7,A8](f:((A1,A2,A3,A4,A5,A6,A7,A8))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8) => a1~a2~a3~a4~a5~a6~a7~a8}
  implicit def flatten9[A1,A2,A3,A4,A5,A6,A7,A8,A9,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9) }
  implicit def tilde9[A1,A2,A3,A4,A5,A6,A7,A8,A9](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9) => a1~a2~a3~a4~a5~a6~a7~a8~a9}
  implicit def flatten10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) }
  implicit def tilde10[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10}
  implicit def flatten11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) }
  implicit def tilde11[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11}
  implicit def flatten12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) }
  implicit def tilde12[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12}
  implicit def flatten13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) }
  implicit def tilde13[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13}
  implicit def flatten14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) }
  implicit def tilde14[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14}
  implicit def flatten15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) }
  implicit def tilde15[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15}
  implicit def flatten16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) }
  implicit def tilde16[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16}
  implicit def flatten17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) }
  implicit def tilde17[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17}
  implicit def flatten18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) }
  implicit def tilde18[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18}
  implicit def flatten19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) }
  implicit def tilde19[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19}
  implicit def flatten20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19~A20) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) }
  implicit def tilde20[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20}
  implicit def flatten21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19~A20~A21) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) }
  implicit def tilde21[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21}
  implicit def flatten22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,R](f:(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22) => R) = (p: A1~A2~A3~A4~A5~A6~A7~A8~A9~A10~A11~A12~A13~A14~A15~A16~A17~A18~A19~A20~A21~A22) => p match { case (a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21~a22) => f(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) }
  implicit def tilde22[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22](f:((A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22))) = f match { case (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22) => a1~a2~a3~a4~a5~a6~a7~a8~a9~a10~a11~a12~a13~a14~a15~a16~a17~a18~a19~a20~a21~a22}
}
