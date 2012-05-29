package pickles

import net.liftweb.json.JsonAST._

object Picklers extends FlattenTilde {
  val  * = Selector.*

  val string  = JsonValue("string"){ case JString(s) => s }(JString(_))
  val int     = JsonValue("int"){ case JInt(i) => i.intValue() }(JInt(_))
  val double  = JsonValue("double"){ case JDouble(d) => d }(JDouble(_))
  val bigint  = JsonValue("bigint"){ case JInt(i) => i }(JInt(_))
  val boolean = JsonValue("boolean"){ case JBool(b) => b}(JBool(_))
  val NULL    = {
    def pickle(a: Null) = JNull
    def unpickle = Parser{ location => 
      location.json match {
        case JNull => Success(null, location)
        case _     => Failure("expected null", location)
      }
    }
    new JsonValue[Null](unpickle, a => Some(pickle(a))){
      def apply[B](value:B):JsonValue[B] = wrap(_ => value)(_ => null)
    }
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

  def unique[A](values:JsonValue[List[A]]) = values.filter(v => v.distinct == v, "expected all elements to be unique")
}