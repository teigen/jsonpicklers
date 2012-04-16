package pickles

import net.liftweb.json.JsonAST._

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
}
case class Success[+A](value:A, location:Location) extends Result[A]
case class Failure(msg:String, location:Location) extends Result[Nothing]

case class ~[+A, +B](_1:A, _2:B){
  def ~[C](c:C) = new ~(this, c)
}

trait Optional[Like[_]]{
  def optional[A](like:Like[A]):Like[Option[A]]
}

object Optional{
  implicit val jsonField = new Optional[JsonField]{
    def optional[A](like: JsonField[A]) = JsonField[Option[A]](like.name, new JsonValue[Option[A]] {
      def unpickle(location:Location) = location.json match {
        case JNothing => Success(None, location)
        case _ => like.value.unpickle(location).map(Some(_))
      }
      def pickle(a: Option[A]) = a.map(like.value.pickle).getOrElse(JNothing)
    })
  }
  
  implicit val jsonValue = new Optional[JsonValue]{
    def optional[A](like: JsonValue[A]) = new JsonValue[Option[A]]{
      def pickle(a: Option[A]) = a.map(like.pickle).getOrElse(JNull)
      def unpickle(location:Location) = location.json match {
        case JNull => Success(None, location)
        case _ => like.unpickle(location).map(Some(_))
      }
    }
  }
}

trait Wrapper[Like[_]]{
  def wrap[A, B](like:Like[A], w:A => B, u:B => A):Like[B]
}

object Wrapper{
  implicit val wrapObject = new Wrapper[JsonObject]{
    def wrap[A, B](like: JsonObject[A], w: (A) => B, u: (B) => A) = new JsonObject[B]{
      def pickle(b: B) = like.pickle(u(b))
      def unpickle(location:Location) = like.unpickle(location).map(w)
    }
  }
  implicit val wrapValue = new Wrapper[JsonValue]{
    def wrap[A, B](like: JsonValue[A], w: (A) => B, u: (B) => A) = new JsonValue[B]{
      def pickle(b: B) = like.pickle(u(b))
      def unpickle(location:Location) = like.unpickle(location).map(w)
    }
  }
  implicit val wrapField = new Wrapper[JsonField]{
    def wrap[A, B](like: JsonField[A], w: (A) => B, u: (B) => A) = JsonField(like.name, Json.wrapper(like.value)(w)(u))
  }
}

trait Or[Like[_]]{
  def or[T, A <: T : Reify, B <: T : Reify](a:Like[A], b:Like[B]):Like[T]
}
object Or {
  implicit val jsonValue = new Or[JsonValue]{
    def or[T, A <: T : Reify, B <: T : Reify](a: JsonValue[A], b: JsonValue[B]) = new JsonValue[T]{
      def pickle(t: T) = implicitly[Reify[A]].reify(t).map(a.pickle).orElse(implicitly[Reify[B]].reify(t).map(b.pickle)).get
      def unpickle(location:Location) = a.unpickle(location).orElse(b.unpickle(location))
    }
  }
}

trait Reify[A]{
  def reify(any:Any):Option[A]
}

object Reify{
  def pf[A](pf:PartialFunction[Any, A]):Reify[A] = new Reify[A] {
    def reify(any: Any) = pf.lift(any)
  }
  
  implicit val string  = pf{ case s:String => s }
  implicit val int     = pf{ case i:Int => i }
  implicit val boolean = pf{ case b:Boolean => b}
}

trait Pickler[A, Pickle <: JValue]{
  def pickle(a:A):Pickle
  def unpickle(location:Location):Result[A]
}

trait JsonValue[A] extends Pickler[A, JValue]{
  def unpickle(json:JValue):Result[A] = unpickle(Root(json))
}
object JsonField{
  implicit def asObject[A](field:JsonField[A]) = new JsonObject[A]{
    def pickle(a: A) = field.pickle(a)
    def unpickle(location:Location) = field.unpickle(location)
  }
}
case class JsonField[A](name:String, value:JsonValue[A]) extends Pickler[A, JObject]{
  def pickle(a: A) = JObject(List(JField(name, value.pickle(a))))
  def unpickle(location:Location) = value.unpickle(location(name))
}
object JsonObject{
  implicit def asValue[A](obj:JsonObject[A]) = new JsonValue[A]{
    def pickle(a: A) = obj.pickle(a)
    def unpickle(location:Location) = obj.unpickle(location)
  }
}
trait JsonObject[A] extends Pickler[A, JObject]

trait JsonObjectSyntaxes {
  class JsonObjectSyntax[A](jsonObject:JsonObject[A]){
    def ~[B] (other:JsonObject[B]) = Json.sequence(jsonObject, other)
    def :: (name:String) = Json.field(name, jsonObject)
  }
  implicit def jsonObject[A](jsonObject:JsonObject[A]) = new JsonObjectSyntax[A](jsonObject)
}

trait JsonFieldSyntaxes {
  class JsonFieldSyntax[A](jsonField:JsonField[A]){
    def ~[B](other:JsonObject[B]) = Json.sequence(jsonField, other)
  }
  implicit def jsonField[A](jsonField:JsonField[A]) = new JsonFieldSyntax[A](jsonField)
}

trait Selector{
  def filter(name:String):Boolean
}

object Selector {
  
  val * = new Selector{ def filter(name:String) = true }
  
  implicit def filter(f:String => Boolean) = new Selector{
    def filter(name:String) = f(name)
  }
}

trait JsonValueSyntaxes {
  class JsonValueSyntax[A](jsonValue:JsonValue[A]){
    def :: (name:String) = Json.field(name, jsonValue)
    def :: (selector:Selector) = Json.select(selector.filter, jsonValue)
    def * = Json.array(jsonValue)
  }
  implicit def jsonValue[A](jsonValue:JsonValue[A]) = new JsonValueSyntax[A](jsonValue) 
}

trait GenericSyntaxes {
  class GenericSyntax[A, Like[_]](like:Like[A]){
    def ? (implicit ev:Optional[Like]) = Json.option(like)
    def | [T, B <: T](b:Like[B])(implicit ev:A <:< T, ra:Reify[A], rb:Reify[B], or:Or[Like]) = Json.or(like, b)
    def wrap[B](w:A => B)(u:B => A)(implicit ev:Wrapper[Like]) = Json.wrapper(like)(w)(u)
  }
  implicit def generic[A, Like[_]](like:Like[A]) = new GenericSyntax[A, Like](like)
}

trait TildeSyntaxes {
  class TildeSyntax[A](a:A){
    def ~[B](b:B) = new ~(a, b)
  }
  implicit def tilde[A](a:A) = new TildeSyntax[A](a)

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

object Json {
  
  object syntax extends JsonFieldSyntaxes with JsonObjectSyntaxes with JsonValueSyntaxes with GenericSyntaxes with TildeSyntaxes
  
  def primitive[A](name:String)(u:PartialFunction[JValue, A])(p:A => JValue):JsonValue[A] = new JsonValue[A]{
    def pickle(a: A) = p(a)
    def unpickle(location:Location) = u.lift(location.json).map(v => Success(v, location)).getOrElse(Failure("expected " + name, location))
  }
  
  val string  = primitive("string"){ case JString(s) => s }(JString(_))
  val int     = primitive("int"){ case JInt(i) => i.intValue() }(JInt(_))
  val boolean = primitive("boolean"){ case JBool(b) => b}(JBool(_))
  
  def array[A](value:JsonValue[A]):JsonValue[List[A]] = new JsonValue[List[A]] {
    def pickle(a: List[A]) = JArray(a.map(value.pickle))
    def unpickle(location:Location) = location.json match {
      case JArray(values) => List.range(0, values.size).foldRight[Result[List[A]]](Success(Nil, location)){
        case (index, lst) => for{
          u <- value.unpickle(location(index))
          tail <- lst
        } yield u :: tail
      }
      case _ => Failure("expected array", location)
    }
  }
  
  def field[A](name:String, value:JsonValue[A]) = JsonField(name, value)
  
  def sequence[A, B](a:JsonObject[A], b:JsonObject[B]):JsonObject[A ~ B] = new JsonObject[~[A, B]] {
    def unpickle(location:Location) = for{
      ua <- a.unpickle(location)
      ub <- b.unpickle(location)
    } yield new ~(ua, ub)
    
    def pickle(ab: ~[A, B]) = {
      val (va ~ vb) = ab
      JObject(a.pickle(va).obj ++ b.pickle(vb).obj)
    }
  }
  
  def wrapper[A, B, Like[_] : Wrapper](like:Like[A])(w:A => B)(u:B => A):Like[B] =
    implicitly[Wrapper[Like]].wrap(like, w, u)
  
  def option[A, Like[_] : Optional](like:Like[A]):Like[Option[A]] = 
    implicitly[Optional[Like]].optional(like)
  
  def or[T, A <: T : Reify, B <: T : Reify, Like[_] : Or](a:Like[A], b:Like[B]):Like[T] = 
    implicitly[Or[Like]].or(a, b)
  
  def select[A](filter:String => Boolean, value:JsonValue[A]):JsonValue[Map[String, A]] = new JsonValue[Map[String, A]] {
    def unpickle(location:Location) = location.json match {
      case JObject(fields) => 
        fields.filter(field => filter(field.name)).foldLeft[Result[Map[String, A]]](Success(Map.empty, location)){
          (map, field) => for{
            m <- map
            v <- value.unpickle(location(field.name))
          } yield m + (field.name -> v)
        }
      case _ => Failure("expected object", location)
    }

    def pickle(a: Map[String, A]) = JObject(a.toList.map{ case (name, v) => JField(name, value.pickle(v)) })
  }
}

object Demo extends App {
  import Json._
  import syntax._
  
  case class Ex(ab:AB, opt:Option[String])
  case class AB(a:List[Int], b:String)
  
  val demo = wrapper(
    sequence(field("ab", wrapper(sequence(
      field("a", array(int)),
      field("b", string))){case a ~ b => AB(a, b)}{ case AB(a, b) => a ~ b }),
    option(field("opt", string)))
  ){ case ab ~ opt => Ex(ab, opt) }{ case Ex(ab, opt) => new ~(ab, opt) }
  
  
  val demo2 = 
    (("ab" ::
      (("a" :: int.*) ~
      ("b" :: string)).wrap(AB)(AB.unapply(_).get)) ~
    ("opt" :: string).?).wrap(Ex)(Ex.unapply(_).get)
  
  val pickled = demo.pickle(Ex(AB(List(1,2,3), "foo"), Some("hello")))
  val unpickled = demo.unpickle(pickled)
  
  println(pickled -> unpickled)
  
  val either:JsonValue[AnyVal] = or(int, boolean)
  
  println(either.unpickle(either.pickle(1)))
  println(either.unpickle(either.pickle(true)))
  
  string.wrap(_.toInt)(_.toString)
  
  val all = Selector.* :: string
  
}
